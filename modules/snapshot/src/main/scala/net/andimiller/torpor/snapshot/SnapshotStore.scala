package net.andimiller.torpor.snapshot

import cats.effect.{Concurrent, Ref}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.Json
import net.andimiller.torpor.core.*
import net.andimiller.torpor.snapshot.Codecs.*
import net.andimiller.hedgehogs.dag.visitor.DagSnapshot
import net.andimiller.hedgehogs.dag.visitor.circe.DagSnapshotCodecs.*

final case class RunId(value: String)
final case class Revision(value: Long)

enum StoreError(val render: String) {
  case NotFound(id: RunId) extends StoreError(s"no stored run ${id.value}")
  case AlreadyExists(id: RunId) extends StoreError(s"a run ${id.value} already exists")
  case Conflict(id: RunId, expected: Revision, actual: Revision)
      extends StoreError(
        s"run ${id.value} is at revision ${actual.value}, not ${expected.value}; someone else got there first"
      )
  case Corrupt(id: RunId, detail: String) extends StoreError(s"stored run ${id.value} cannot be read: $detail")
}

/** A stored run: the plan (for assembly on the far side), the hedgehogs snapshot, and the schema it was planned
  * against. `revision` is the compare-and-swap token.
  */
final case class StoredRun(
    schemaHash: String,
    plan: Plan,
    run: DagSnapshot[NodeId, PlanNode, NodeResult, Json, Unit],
    revision: Revision,
    /** True between a resume claiming the run and that resume finishing or suspending again. A claimed run cannot be
      * claimed, which is what stops two webhooks running the same downstream work.
      */
    claimed: Boolean = false
)

object StoredRun {
  val Version: Int = 1

  def toJson(stored: StoredRun): Json =
    Json.obj(
      "version"    := Version,
      "schemaHash" := stored.schemaHash,
      "plan"       := stored.plan,
      "run"        := stored.run,
      "revision"   := stored.revision.value,
      "claimed"    := stored.claimed
    )

  def fromJson(json: Json): Either[String, StoredRun] = {
    val cursor = json.hcursor
    for {
      version  <- cursor.get[Int]("version").left.map(_.message)
      _        <- Either.cond(version == Version, (), s"snapshot version $version, expected $Version")
      hash     <- cursor.get[String]("schemaHash").left.map(_.message)
      plan     <- cursor.get[Plan]("plan").left.map(_.message)
      run      <- cursor.get[DagSnapshot[NodeId, PlanNode, NodeResult, Json, Unit]]("run").left.map(_.message)
      revision <- cursor.get[Long]("revision").left.map(_.message)
      claimed  <- cursor.get[Boolean]("claimed").orElse(Right(false)).left.map(_.message)
    } yield StoredRun(hash, plan, run, Revision(revision), claimed)
  }
}

/** Compare-and-swap storage for suspended runs.
  *
  * `replace` carries the revision the caller read. Two webhooks racing to resume the same run cannot both proceed: the
  * loser gets `Conflict` and reloads. That is what makes "exactly one writer per run" true rather than hopeful.
  */
trait SnapshotStore[F[_]] {
  def create(id: RunId, run: StoredRun): F[Either[StoreError, Revision]]
  def load(id: RunId): F[Option[StoredRun]]
  def replace(id: RunId, expected: Revision, run: StoredRun): F[Either[StoreError, Revision]]
  def delete(id: RunId, expected: Revision): F[Either[StoreError, Unit]]

  /** Atomically take a suspended run: the returned copy is claimed at a new revision. Exactly one caller wins a
    * suspended run; everyone else gets `Conflict`.
    */
  def claim(id: RunId): F[Either[StoreError, StoredRun]]
}

object SnapshotStore {

  /** The M1 implementation. The Postgres one is `UPDATE ... WHERE revision = ?` and lands next; this one exists so the
    * contract and the restart test do not wait for a database.
    */
  def inMemory[F[_]: Concurrent]: F[SnapshotStore[F]] =
    Ref.of[F, Map[RunId, StoredRun]](Map.empty).map(new InMemory[F](_))

  /** Postgres, compare-and-swap on the `revision` column. Schema: `torpor-snapshot.sql`. */
  def skunk[F[_]: Concurrent](session: _root_.skunk.Session[F]): SnapshotStore[F] =
    new SkunkSnapshotStore[F](session)

  private final class InMemory[F[_]: Concurrent](runs: Ref[F, Map[RunId, StoredRun]]) extends SnapshotStore[F] {

    def create(id: RunId, run: StoredRun): F[Either[StoreError, Revision]] =
      runs.modify { current =>
        if (current.contains(id)) (current, Left(StoreError.AlreadyExists(id)))
        else {
          val stored = run.copy(revision = Revision(1))
          (current.updated(id, stored), Right(stored.revision))
        }
      }

    def load(id: RunId): F[Option[StoredRun]] = runs.get.map(_.get(id))

    def replace(id: RunId, expected: Revision, run: StoredRun): F[Either[StoreError, Revision]] =
      runs.modify { current =>
        current.get(id) match {
          case None                                            => (current, Left(StoreError.NotFound(id)))
          case Some(existing) if existing.revision != expected =>
            (current, Left(StoreError.Conflict(id, expected, existing.revision)))
          case Some(existing)                                  =>
            val stored = run.copy(revision = Revision(existing.revision.value + 1))
            (current.updated(id, stored), Right(stored.revision))
        }
      }

    def claim(id: RunId): F[Either[StoreError, StoredRun]] =
      runs.modify { current =>
        current.get(id) match {
          case None                               => (current, Left(StoreError.NotFound(id)))
          case Some(existing) if existing.claimed =>
            (current, Left(StoreError.Conflict(id, existing.revision, existing.revision)))
          case Some(existing)                     =>
            val claimed = existing.copy(revision = Revision(existing.revision.value + 1), claimed = true)
            (current.updated(id, claimed), Right(claimed))
        }
      }

    def delete(id: RunId, expected: Revision): F[Either[StoreError, Unit]] =
      runs.modify { current =>
        current.get(id) match {
          case None                                            => (current, Left(StoreError.NotFound(id)))
          case Some(existing) if existing.revision != expected =>
            (current, Left(StoreError.Conflict(id, expected, existing.revision)))
          case Some(_)                                         => (current - id, Right(()))
        }
      }
  }
}
