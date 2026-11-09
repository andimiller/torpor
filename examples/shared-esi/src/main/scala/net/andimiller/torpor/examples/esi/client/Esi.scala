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

/** `characters/affiliation/`: the batch endpoint that makes a character list cost two requests, not two per row. */
final case class EsiAffiliation(
    characterId: Long,
    corporationId: Long,
    allianceId: Option[Long],
    factionId: Option[Long]
) derives ConfiguredCodec

/** `universe/names/` answers for any mix of ids: systems, constellations, regions, characters, corporations. */
final case class EsiName(id: Long, name: String) derives ConfiguredCodec

final case class EsiSolarSystem(id: Long, name: String, constellationId: Long) derives ConfiguredCodec

final case class EsiConstellation(id: Long, name: String, regionId: Long) derives ConfiguredCodec

final case class EsiRegion(id: Long, name: String) derives ConfiguredCodec

/** Every call the examples make, batched by construction. A fake implements this in tests; nothing else touches HTTP.
  */
trait EsiApi[F[_]] {
  def characterIdByName(name: String): F[Option[Long]]
  def characters(ids: NonEmptyList[Long]): F[Map[Long, EsiCharacter]]
  def corporations(ids: NonEmptyList[Long]): F[Map[Long, EsiCorporation]]
  def alliances(ids: NonEmptyList[Long]): F[Map[Long, EsiAlliance]]

  /** A route is computed by ESI in one call; `None` when ESI knows no route, which is not an error. */
  def route(origin: Long, destination: Long, flag: Option[String]): F[Option[List[Long]]]
  def systems(ids: NonEmptyList[Long]): F[Map[Long, EsiSolarSystem]]
  def constellations(ids: NonEmptyList[Long]): F[Map[Long, EsiConstellation]]
  def names(ids: NonEmptyList[Long]): F[Map[Long, String]]
}

object EsiApi {

  /** Unauthenticated ESI over http4s, with the User-Agent set on every request and 404 read as absence. */
  def http[F[_]: Concurrent](client: Client[F], config: EsiConfig): EsiApi[F] =
    new EsiApi[F] {

      // slashes inside a single segment get percent-encoded, so the path must be added segment by segment
      private def request(method: Method, path: String, query: Map[String, String] = Map.empty): Request[F] = {
        val uri = path.split("/").filter(_.nonEmpty).foldLeft(config.baseUri / "latest")(_ / _)
        Request[F](method, if (query.isEmpty) uri else uri.withQueryParams(query))
          .putHeaders(Header.Raw(ci"User-Agent", config.userAgent), Header.Raw(ci"Accept", "application/json"))
      }

      private def getJson(path: String, query: Map[String, String] = Map.empty): F[Option[Json]] =
        client.run(request(Method.GET, path, query)).use {
          case response if response.status == Status.NotFound => Concurrent[F].pure(None)
          case response if response.status.isSuccess          => response.as[Json].map(Some(_))
          case response                                       =>
            Concurrent[F].raiseError(new RuntimeException(s"ESI $path returned ${response.status.code}"))
        }

      private def postJson(path: String, body: Json): F[Json] =
        client.run(request(Method.POST, path).withEntity(body)(jsonEncoderOf[F, Json])).use {
          case response if response.status.isSuccess => response.as[Json]
          case response                              =>
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

      def characterIdByName(name: String): F[Option[Long]]                                 =
        postJson("universe/ids/", Json.arr(Json.fromString(name)))
          .map(_.hcursor.downField("characters").downArray.get[Long]("id").toOption)

      // both endpoints are real batches, so any number of characters is two requests, never two per row
      // shortcut: ESI caps these bodies at 1000 ids, chunk if a query can exceed that
      def affiliations(ids: NonEmptyList[Long]): F[Map[Long, EsiAffiliation]] =
        postJson("characters/affiliation/", idsJson(ids))
          .flatMap(decode[List[EsiAffiliation]])
          .map(_.map(row => row.characterId -> row).toMap)

      def names(ids: NonEmptyList[Long]): F[Map[Long, String]]                =
        postJson("universe/names/", idsJson(ids))
          .flatMap(decode[List[EsiName]])
          .map(_.map(row => row.id -> row.name).toMap)

      private def idsJson(ids: NonEmptyList[Long]): Json                      =
        Json.fromValues(ids.toList.distinct.map(Json.fromLong))

      def characters(ids: NonEmptyList[Long]): F[Map[Long, EsiCharacter]] =
        (affiliations(ids), names(ids)).mapN { (affiliations, names) =>
          ids.toList.distinct
            .flatMap(id =>
              (affiliations.get(id), names.get(id)).mapN((affiliation, name) =>
                id -> EsiCharacter(id, name, affiliation.corporationId)
              )
            )
            .toMap
        }

      def corporations(ids: NonEmptyList[Long]): F[Map[Long, EsiCorporation]] = byId("corporations", ids)
      def alliances(ids: NonEmptyList[Long]): F[Map[Long, EsiAlliance]]       = byId("alliances", ids)

      def route(origin: Long, destination: Long, flag: Option[String]): F[Option[List[Long]]] =
        getJson(s"route/$origin/$destination/", flag.fold(Map.empty[String, String])(f => Map("flag" -> f)))
          .flatMap(_.traverse(decode[List[Long]]))

      // ESI has no batch endpoint for systems or constellations, so these two levels cost one request per key
      def systems(ids: NonEmptyList[Long]): F[Map[Long, EsiSolarSystem]]          = byId("universe/systems", ids)
      def constellations(ids: NonEmptyList[Long]): F[Map[Long, EsiConstellation]] = byId("universe/constellations", ids)
    }
}
