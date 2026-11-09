package net.andimiller.torpor.examples.esi

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import com.comcast.ip4s.*
import io.circe.Decoder
import net.andimiller.torpor.examples.esi.client.*
import net.andimiller.torpor.http.TorporHttp
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.schema.Schema
import net.andimiller.torpor.source.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router

final case class CharacterArgs(id: Option[Long], name: Option[String]) derives Decoder
final case class CharacterIds(ids: List[Long]) derives Decoder

/** The whole GraphQL side: two sources, which is all the "optional hydration" needs.
  *
  *   - `characters` owns `Character` and the root lookup
  *   - `universe` extends `Character` with `corporation`, and `Corporation` with `alliance`
  */
object EsiGraphQL {

  val characters =
    schema"""type Character { id: ID! name: String! }
      |extend type Query {
      |  character(id: ID, name: String): Character
      |  characters(ids: [ID!]!): [Character!]!
      |}
      |"""

  val universe =
    schema"""type Corporation { id: ID! name: String! ticker: String! }
      |type Alliance { id: ID! name: String! ticker: String! }
      |extend type Character { corporation: Corporation! }
      |extend type Corporation { alliance: Alliance }
      |"""

  def sources(api: EsiApi[IO]): List[Source[IO]] = {
    val charactersSource = Source[IO](
      "characters",
      characters.text,
      Resolvers[IO](
        resolver.root[IO](characters.coordinate("Query", "character")) { (args: CharacterArgs) =>
          (args.id, args.name) match {
            case (Some(id), None)   => api.characters(NonEmptyList.one(id)).map(_.get(id))
            case (None, Some(name)) =>
              api.characterIdByName(name).flatMap {
                case None     => IO.pure(None)
                case Some(id) => api.characters(NonEmptyList.one(id)).map(_.get(id))
              }
            case _                  =>
              IO.raiseError(new IllegalArgumentException("give exactly one of 'id' or 'name'"))
          }
        },
        // one characters call, however many ids: the corporation node below then batches too
        resolver.root[IO](characters.coordinate("Query", "characters")) { (args: CharacterIds) =>
          NonEmptyList.fromList(args.ids) match {
            case None      => IO.pure(Nil)
            case Some(ids) => api.characters(ids).map(byId => args.ids.flatMap(byId.get))
          }
        }
      )
    )

    val universeSource = Source[IO](
      "universe",
      universe.text,
      Resolvers[IO](
        // one call per level: every selected character's corporation comes from a single request
        resolver
          .field[IO](universe.coordinate("Character", "corporation"))
          .key((character: EsiCharacter) => character.corporationId)
          .batch((ids: NonEmptyList[Long]) => api.corporations(ids)),
        // ESI gives a corporation zero or one alliance: an optional key resolves to null, never to an error
        resolver
          .optionalField[IO](universe.coordinate("Corporation", "alliance"))
          .key((corporation: EsiCorporation) => corporation.allianceId)
          .batch((ids: NonEmptyList[Long]) => api.alliances(ids))
      )
    )

    List(charactersSource, universeSource)
  }

  def torpor(api: EsiApi[IO]): Torpor[IO] = Torpor[IO](sources(api): _*)

  def schema(api: EsiApi[IO]): Schema =
    torpor(api).schema.fold(
      errors => throw new IllegalStateException(errors.toChain.toList.map(_.render).mkString("; ")),
      identity
    )
}

/** `sbt "esi/run"`, then POST to http://localhost:8080/graphql:
  *
  * { character(name: "Rex") { name corporation { name alliance { name } } } }
  */
object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    EmberClientBuilder
      .default[IO]
      .build
      .use { client =>
        val api = EsiApi.http[IO](client, EsiConfig(userAgent = "torpor-example/0.1 (github.com/andimiller/torpor)"))

        EsiGraphQL.torpor(api).startup.flatMap { case (torpor, schema) =>
          EmberServerBuilder
            .default[IO]
            .withHost(host"0.0.0.0")
            .withPort(port"8080")
            .withHttpApp(Router("/" -> TorporHttp.routes(torpor, schema)).orNotFound)
            .build
            .useForever
        }
      }
      .handleErrorWith(error => IO.println(error.getMessage).as(ExitCode.Error))
}
