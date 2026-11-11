package net.andimiller.torpor.examples.esi

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import com.comcast.ip4s.*
import net.andimiller.torpor.examples.esi.client.*
import net.andimiller.torpor.http.TorporHttp
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.schema.Schema
import net.andimiller.torpor.source.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router

/** The whole GraphQL side: three sources, which is all the "optional hydration" and the fan-out need.
  *
  *   - `characters` owns `Character` and the root lookup
  *   - `universe` extends `Character` with `corporation`, `Corporation` with `alliance`, and the route's systems down
  *     to their region
  *   - `routes` owns the route lookup and the two types it returns
  */
object EsiGraphQL {

  // ESI ids are numbers, so every `ID` argument decodes as a Long; the macro reads this where the SDL is written
  given Scalar["ID", Long]()

  val characters =
    schema"""type Character { id: ID! name: String! }
      |extend type Query {
      |  character(id: ID, name: String): Character
      |  characters(ids: [ID!]!): [Character!]!
      |}
      |"""

  // Each source's type records what its resolvers produce and decode, so `torpor` below can check, when this
  // compiles, that every keyed field can read its key from whatever another source produces for its parent.

  /** Owns `Character`: both roots produce `Character` values as `EsiCharacter`. */
  def charactersSource(api: EsiApi[IO]) =
    Source[IO]("characters", characters)(
      resolver.root[IO](characters.Query.character) { args =>
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
      resolver.root[IO](characters.Query.characters) { args =>
        NonEmptyList.fromList(args.ids) match {
          case None      => IO.pure(Nil)
          case Some(ids) => api.characters(ids).map(byId => args.ids.flatMap(byId.get))
        }
      }
    )

  val universe =
    schema"""type Corporation { id: ID! name: String! ticker: String! }
      |type Alliance { id: ID! name: String! ticker: String! }
      |type Region { id: ID! name: String! }
      |extend type Character { corporation: Corporation! }
      |extend type Corporation { alliance: Alliance }
      |extend type SolarSystem { constellation: Constellation! }
      |extend type Constellation { region: Region! }
      |"""

  /** Hydrates `Character.corporation` from each character's `corporationId`, `Corporation.alliance` likewise, and the
    * route's systems down to their region.
    */
  def universeSource(api: EsiApi[IO]) =
    Source[IO]("universe", universe)(
      // one call per level: every selected character's corporation comes from a single request
      resolver
        .field[IO](universe.Character.corporation)
        .key((character: EsiCharacter) => character.corporationId)
        .batch((ids: NonEmptyList[Long]) => api.corporations(ids)),
      // ESI gives a corporation zero or one alliance: an optional key resolves to null, never to an error
      resolver
        .optionalField[IO](universe.Corporation.alliance)
        .key((corporation: EsiCorporation) => corporation.allianceId)
        .batch((ids: NonEmptyList[Long]) => api.alliances(ids)),
      // one constellations call per level, however many systems are on the route
      resolver
        .field[IO](universe.SolarSystem.constellation)
        .key((system: EsiSolarSystem) => system.constellationId)
        .batch((ids: NonEmptyList[Long]) => api.constellations(ids)),
      // names is a batch endpoint, so the last level collapses into one call for every region on the route
      resolver
        .field[IO](universe.Constellation.region)
        .key((constellation: EsiConstellation) => constellation.regionId)
        .batch((ids: NonEmptyList[Long]) => api.names(ids).map(_.map { case (id, name) => id -> EsiRegion(id, name) }))
    )

  // the route is one ESI call that returns N system ids: the fan-out the planner exists for, and the only query here
  // that is several levels deep. The two types it returns are defined here; the fields that hydrate them are extensions
  // in `universe`, which is what makes them plan nodes rather than inline reads.
  val routes =
    schema"""type SolarSystem { id: ID! name: String! }
      |type Constellation { id: ID! name: String! }
      |extend type Query { route(origin: ID!, destination: ID!, flag: String): [SolarSystem!] }
      |"""

  /** Owns the route lookup: the root produces `SolarSystem` values as `EsiSolarSystem`. */
  def routesSource(api: EsiApi[IO]) =
    Source[IO]("routes", routes)(
      // the route ids are the root's own fan-out: ESI computes the route, then each system is its own request
      resolver.root[IO](routes.Query.route) { args =>
        api.route(args.origin, args.destination, args.flag).flatMap {
          case None      => IO.pure(Option.empty[List[EsiSolarSystem]])
          case Some(ids) =>
            NonEmptyList.fromList(ids.distinct) match {
              case None      => IO.pure(Option(List.empty[EsiSolarSystem]))
              case Some(nel) => api.systems(nel).map(byId => Option(ids.flatMap(byId.get)))
            }
        }
      }
    )

  def torpor(api: EsiApi[IO]): Torpor[IO] =
    Torpor[IO](universeSource(api), charactersSource(api), routesSource(api))

  def schema(api: EsiApi[IO]): Schema =
    torpor(api).schema.fold(
      errors => throw new IllegalStateException(errors.toChain.toList.map(_.render).mkString("; ")),
      identity
    )
}

/** `sbt "esi/run"`, then POST to http://localhost:8080/graphql:
  *
  * { character(name: "Rex") { name corporation { name alliance { name } } } }
  *
  * and to the same address plus `/explain` for the plan behind a query:
  *
  * { route(origin: 30000142, destination: 30002187) { name constellation { region { name } } } }
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
            .withHttpApp(
              Router("/" -> (TorporHttp.routes(torpor, schema) <+> ExplainRoute.routes(schema))).orNotFound
            )
            .build
            .useForever
        }
      }
      .handleErrorWith(error => IO.println(error.getMessage).as(ExitCode.Error))
}
