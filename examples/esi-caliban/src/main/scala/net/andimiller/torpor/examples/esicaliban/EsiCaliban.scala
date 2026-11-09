package net.andimiller.torpor.examples.esicaliban

import caliban.interop.tapir.HttpInterpreter
import caliban.schema.{ArgBuilder, Schema}
import caliban.{GraphQL, Http4sAdapter, RootResolver, graphQL}
import cats.data.NonEmptyList
import com.comcast.ip4s.{host, port}
import net.andimiller.torpor.examples.esi.client.*
import org.http4s.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import zio.*
import zio.interop.catz.*

final case class Alliance(id: Long, name: String, ticker: String) derives Schema.SemiAuto

final case class Corporation(id: Long, name: String, ticker: String, alliance: UIO[Option[Alliance]])
    derives Schema.SemiAuto

final case class Character(id: Long, name: String, corporation: UIO[Corporation]) derives Schema.SemiAuto

final case class CharacterArgs(id: Option[Long], name: Option[String]) derives Schema.SemiAuto, ArgBuilder
final case class CharacterIds(ids: List[Long]) derives Schema.SemiAuto, ArgBuilder

final case class Query(
    character: CharacterArgs => UIO[Option[Character]],
    characters: CharacterIds => UIO[List[Character]]
) derives Schema.SemiAuto

object EsiCaliban {

  def graphqlFor(esi: EsiApi[Task]): GraphQL[Any] = {

    def allianceOf(id: Long): UIO[Option[Alliance]] =
      esi
        .alliances(NonEmptyList.one(id))
        .orDie
        .map(_.get(id).map(alliance => Alliance(alliance.id, alliance.name, alliance.ticker)))

    def corporationOf(id: Long): UIO[Corporation] =
      esi
        .corporations(NonEmptyList.one(id))
        .orDie
        .map(_.get(id))
        .flatMap {
          case None        => ZIO.dieMessage(s"ESI has no corporation $id")
          case Some(found) =>
            ZIO.succeed(Corporation(found.id, found.name, found.ticker, found.allianceId.fold(ZIO.none)(allianceOf)))
        }

    def characterFrom(found: EsiCharacter): Character =
      Character(found.id, found.name, corporationOf(found.corporationId))

    def characterOf(id: Long): UIO[Option[Character]] =
      esi
        .characters(NonEmptyList.one(id))
        .orDie
        .map(_.get(id))
        .map(_.map(characterFrom))

    graphQL(
      RootResolver(
        Query(
          character = { args =>
            (args.id, args.name) match {
              case (Some(id), None)   => characterOf(id)
              case (None, Some(name)) => esi.characterIdByName(name).orDie.flatMap(_.fold(ZIO.none)(characterOf))
              case _                  => ZIO.dieMessage("give exactly one of 'id' or 'name'")
            }
          },
          characters = { args =>
            NonEmptyList.fromList(args.ids) match {
              case None      => ZIO.succeed(Nil)
              case Some(ids) => esi.characters(ids).orDie.map(byId => args.ids.flatMap(byId.get).map(characterFrom))
            }
          }
        )
      )
    )
  }
}

/** `sbt "esiCaliban/run"`, then POST to http://localhost:8080/graphql */
object Main extends ZIOAppDefault {

  private val config = EsiConfig(userAgent = "torpor-example/0.1 (github.com/andimiller/torpor)")

  def run =
    EmberClientBuilder
      .default[Task]
      .build
      .use { client =>
        EsiCaliban.graphqlFor(EsiApi.http[Task](client, config)).interpreter.flatMap { interpreter =>
          EmberServerBuilder
            .default[Task]
            .withHost(host"0.0.0.0")
            .withPort(port"8080")
            .withHttpApp(Http4sAdapter.makeHttpService(HttpInterpreter(interpreter)).orNotFound)
            .build
            .useForever
        }
      }
}
