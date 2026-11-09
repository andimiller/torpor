package net.andimiller.torpor.snapshot

import cats.effect.Concurrent
import cats.syntax.all._
import io.circe.parser
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

/** Postgres implementation with skunk.
  *
  * The CAS contract is the point: every write carries the revision it read, so two webhooks racing to resume one run
  * cannot both run downstream work. `UPDATE ... WHERE revision = ?` is the whole mechanism; a lost update returns
  * `Conflict` and the caller reloads.
  *
  * `payload` is the `StoredRun` JSON as text; the `revision` column is authoritative and overrides whatever the payload
  * carries, so there is one source of truth for the CAS token. JSONB would buy queryability nobody needs here, and
  * would mean a codec dependency for it.
  *
  * Schema: `modules/snapshot/src/main/resources/torpor-snapshot.sql`.
  */
final class SkunkSnapshotStore[F[_]: Concurrent](session: Session[F]) extends SnapshotStore[F] {

  private val createQuery: Query[(String, String), Long] =
    sql"""INSERT INTO torpor_runs (id, revision, payload) VALUES ($text, 1, $text)
          ON CONFLICT (id) DO NOTHING
          RETURNING revision""".query(int8)

  private val loadQuery: Query[String, (Long, String)] =
    sql"SELECT revision, payload FROM torpor_runs WHERE id = $text".query(int8 ~ text)

  private val replaceQuery: Query[(String, String, Long), Long] =
    sql"""UPDATE torpor_runs SET revision = revision + 1, payload = $text, updated_at = now()
          WHERE id = $text AND revision = $int8
          RETURNING revision""".query(int8)

  private val deleteQuery: Query[(String, Long), Long] =
    sql"DELETE FROM torpor_runs WHERE id = $text AND revision = $int8 RETURNING revision".query(int8)

  private val claimQuery: Query[String, (Long, String)] =
    sql"""UPDATE torpor_runs SET revision = revision + 1, claimed = true, updated_at = now()
          WHERE id = $text AND claimed = false
          RETURNING revision, payload""".query(int8 ~ text)

  private val revisionQuery: Query[String, Long] =
    sql"SELECT revision FROM torpor_runs WHERE id = $text".query(int8)

  def create(id: RunId, run: StoredRun): F[Either[StoreError, Revision]] =
    session
      .prepare(createQuery)
      .flatMap(_.option((id.value, StoredRun.toJson(run).noSpaces)))
      .map(_.toRight(StoreError.AlreadyExists(id)).map(Revision(_)))

  def load(id: RunId): F[Option[StoredRun]] =
    session
      .prepare(loadQuery)
      .flatMap(_.option(id.value))
      .flatMap {
        case None                      => Concurrent[F].pure(None)
        case Some((revision, payload)) =>
          Concurrent[F]
            .fromEither(
              parse(id, revision, payload).leftMap(error => new IllegalStateException(error.render))
            )
            .map(Some(_))
      }

  def claim(id: RunId): F[Either[StoreError, StoredRun]] =
    session
      .prepare(claimQuery)
      .flatMap(_.option(id.value))
      .flatMap {
        case Some((revision, payload)) =>
          Concurrent[F]
            .fromEither(parse(id, revision, payload).leftMap(error => new IllegalStateException(error.render)))
            .map(claimed => Right(claimed.copy(claimed = true)))
        case None                      =>
          disambiguate(id, Revision(0)).flatMap {
            case Left(error) => Concurrent[F].pure(Left(error))
            case Right(_)    => Concurrent[F].pure(Left(StoreError.NotFound(id)))
          }
      }

  def replace(id: RunId, expected: Revision, run: StoredRun): F[Either[StoreError, Revision]] =
    session
      .prepare(replaceQuery)
      .flatMap(_.option((StoredRun.toJson(run).noSpaces, id.value, expected.value)))
      .flatMap {
        case Some(revision) => Concurrent[F].pure(Right(Revision(revision)))
        case None           => disambiguate(id, expected)
      }

  def delete(id: RunId, expected: Revision): F[Either[StoreError, Unit]] =
    session
      .prepare(deleteQuery)
      .flatMap(_.option((id.value, expected.value)))
      .flatMap {
        case Some(_) => Concurrent[F].pure(Right(()))
        case None    => disambiguate(id, expected).map(_.map(_ => ()))
      }

  /** No row changed: either the run is gone, or someone bumped the revision first. One extra read tells them apart. */
  private def disambiguate(id: RunId, expected: Revision): F[Either[StoreError, Revision]] =
    session
      .prepare(revisionQuery)
      .flatMap(_.option(id.value))
      .map {
        case None         => Left(StoreError.NotFound(id))
        case Some(actual) => Left(StoreError.Conflict(id, expected, Revision(actual)))
      }

  private def parse(id: RunId, revision: Long, payload: String): Either[StoreError, StoredRun] =
    parser
      .parse(payload)
      .leftMap(error => StoreError.Corrupt(id, error.message))
      .flatMap(json => StoredRun.fromJson(json).leftMap(error => StoreError.Corrupt(id, error)))
      .map(_.copy(revision = Revision(revision)))
}
