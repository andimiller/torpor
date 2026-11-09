package net.andimiller.torpor.examples.esi.client

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json}
import io.circe.derivation.{ConfiguredCodec, Configuration}
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.implicits.*
import org.typelevel.ci.CIStringSyntax

/** ESI requires a descriptive User-Agent; unauthenticated endpoints need nothing else. */
final case class EsiConfig(userAgent: String, baseUri: Uri = uri"https://esi.evetech.net")

given Configuration = Configuration.default.withSnakeCaseMemberNames

/** The carrier fields (`corporation_id`, `alliance_id`) travel in the JSON between nodes but never appear in the SDL.
  */
final case class EsiCharacter(id: Long, name: String, corporationId: Long) derives ConfiguredCodec

final case class EsiCorporation(id: Long, name: String, ticker: String, allianceId: Option[Long])
    derives ConfiguredCodec

final case class EsiAlliance(id: Long, name: String, ticker: String) derives ConfiguredCodec

/** Every call the examples make, batched by construction. A fake implements this in tests; nothing else touches HTTP.
  */
trait EsiApi[F[_]] {
  def characterIdByName(name: String): F[Option[Long]]
  def characters(ids: NonEmptyList[Long]): F[Map[Long, EsiCharacter]]
  def corporations(ids: NonEmptyList[Long]): F[Map[Long, EsiCorporation]]
  def alliances(ids: NonEmptyList[Long]): F[Map[Long, EsiAlliance]]
}

object EsiApi {

  /** Unauthenticated ESI over http4s, with the User-Agent set on every request and 404 read as absence. */
  def http[F[_]: Concurrent](client: Client[F], config: EsiConfig): EsiApi[F] =
    new EsiApi[F] {

      // slashes inside a single segment get percent-encoded, so the path must be added segment by segment
      private def request(method: Method, path: String): Request[F] =
        Request[F](method, path.split("/").filter(_.nonEmpty).foldLeft(config.baseUri / "latest")(_ / _))
          .putHeaders(Header.Raw(ci"User-Agent", config.userAgent), Header.Raw(ci"Accept", "application/json"))

      private def getJson(path: String): F[Option[Json]] =
        client.run(request(Method.GET, path)).use {
          case response if response.status == Status.NotFound => Concurrent[F].pure(None)
          case response if response.status.isSuccess          => response.as[Json].map(Some(_))
          case response                                       =>
            Concurrent[F].raiseError(new RuntimeException(s"ESI $path returned ${response.status.code}"))
        }

      private def decode[A: Decoder](json: Json): F[A] =
        Decoder[A].decodeJson(json) match {
          case Left(error)  =>
            Concurrent[F].raiseError(
              new RuntimeException(s"ESI response did not decode: ${error.message} at ${error.history.mkString(".")}")
            )
          case Right(value) => Concurrent[F].pure(value)
        }

      // these ESI endpoints omit the id (it's in the path), so put the one we asked for back in the row
      private def byId[A: Decoder](kind: String, ids: NonEmptyList[Long]): F[Map[Long, A]] =
        ids.toList.distinct
          .traverse { id =>
            def withId(json: Json): Json = Json.obj("id" -> Json.fromLong(id)).deepMerge(json)
            getJson(s"$kind/$id/").flatMap(_.traverse(json => decode[A](withId(json))).map(id -> _))
          }
          .map(_.collect { case (id, Some(value)) => id -> value }.toMap)

      def characterIdByName(name: String): F[Option[Long]] = {
        val body = Json.arr(Json.fromString(name))
        client
          .run(request(Method.POST, "universe/ids/").withEntity(body)(jsonEncoderOf[F, Json]))
          .use {
            case response if response.status.isSuccess => response.as[Json]
            case response                              =>
              Concurrent[F].raiseError(new RuntimeException(s"ESI universe/ids returned ${response.status.code}"))
          }
          .map(_.hcursor.downField("characters").downArray.get[Long]("id").toOption)
      }

      def characters(ids: NonEmptyList[Long]): F[Map[Long, EsiCharacter]]     = byId("characters", ids)
      def corporations(ids: NonEmptyList[Long]): F[Map[Long, EsiCorporation]] = byId("corporations", ids)
      def alliances(ids: NonEmptyList[Long]): F[Map[Long, EsiAlliance]]       = byId("alliances", ids)
    }
}
