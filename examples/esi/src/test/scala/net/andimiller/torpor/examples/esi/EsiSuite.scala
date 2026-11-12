package net.andimiller.torpor.examples.esi

import cats.data.NonEmptyList
import cats.effect.IO
import net.andimiller.torpor.examples.esi.client.*
import net.andimiller.torpor.core.{PathSegment, Response as GraphQLResponse}
import net.andimiller.torpor.source.{Planner, RunOutcome, Torpor}
import net.andimiller.torpor.schema.Executable
import io.circe.Json
import org.http4s.{HttpApp, Method, Response, Status, Uri}
import org.http4s.circe.*
import org.http4s.client.Client

import java.util.concurrent.atomic.AtomicInteger

/** Exercises the example end to end with a fake ESI: no network, and call counts prove the batching and the optional
  * hydration.
  */
final class EsiSuite extends munit.CatsEffectSuite {

  private final class FakeEsi(
      charactersById: Map[Long, EsiCharacter],
      namesToIds: Map[String, Long],
      corporationsById: Map[Long, EsiCorporation],
      alliancesById: Map[Long, EsiAlliance],
      routeByEnds: Map[(Long, Long), List[Long]] = Map.empty,
      systemsById: Map[Long, EsiSolarSystem] = Map.empty,
      constellationsById: Map[Long, EsiConstellation] = Map.empty,
      namesById: Map[Long, String] = Map.empty
  ) extends EsiApi[IO] {
    val characterCalls    = new AtomicInteger(0)
    val nameLookups       = new AtomicInteger(0)
    val corporationCalls  = new AtomicInteger(0)
    val allianceCalls     = new AtomicInteger(0)
    val corporationKeys   = new AtomicInteger(0)
    val routeCalls        = new AtomicInteger(0)
    val systemKeys        = new AtomicInteger(0)
    val constellationKeys = new AtomicInteger(0)
    val nameKeys          = new AtomicInteger(0)

    def characterIdByName(name: String): IO[Option[Long]] =
      IO { nameLookups.incrementAndGet(); namesToIds.get(name) }

    def characters(ids: NonEmptyList[Long]): IO[Map[Long, EsiCharacter]]     =
      IO { characterCalls.incrementAndGet(); ids.toList.flatMap(id => charactersById.get(id).map(id -> _)).toMap }

    def corporations(ids: NonEmptyList[Long]): IO[Map[Long, EsiCorporation]] =
      IO {
        corporationCalls.incrementAndGet()
        corporationKeys.set(ids.size)
        ids.toList.flatMap(id => corporationsById.get(id).map(id -> _)).toMap
      }

    def alliances(ids: NonEmptyList[Long]): IO[Map[Long, EsiAlliance]]                       =
      IO { allianceCalls.incrementAndGet(); ids.toList.flatMap(id => alliancesById.get(id).map(id -> _)).toMap }

    def route(origin: Long, destination: Long, flag: Option[String]): IO[Option[List[Long]]] =
      IO { routeCalls.incrementAndGet(); routeByEnds.get(origin -> destination) }

    def systems(ids: NonEmptyList[Long]): IO[Map[Long, EsiSolarSystem]]                      =
      IO {
        systemKeys.set(ids.size)
        ids.toList.flatMap(id => systemsById.get(id).map(id -> _)).toMap
      }

    def constellations(ids: NonEmptyList[Long]): IO[Map[Long, EsiConstellation]] =
      IO {
        constellationKeys.set(ids.size)
        ids.toList.flatMap(id => constellationsById.get(id).map(id -> _)).toMap
      }

    def names(ids: NonEmptyList[Long]): IO[Map[Long, String]] =
      IO { nameKeys.set(ids.size); ids.toList.flatMap(id => namesById.get(id).map(id -> _)).toMap }
  }

  private def fake(withAlliance: Boolean = true): FakeEsi =
    new FakeEsi(
      charactersById = Map(
        1001L -> EsiCharacter(1001L, "Rex", 2001L),
        1002L -> EsiCharacter(1002L, "Mochi", 2002L),
        1003L -> EsiCharacter(1003L, "Biscuit", 2001L)
      ),
      namesToIds = Map("Rex" -> 1001L),
      corporationsById = Map(
        2001L -> EsiCorporation(2001L, "Hedgerow Holdings", "HEDGE", if (withAlliance) Some(3001L) else None),
        2002L -> EsiCorporation(2002L, "Mochi Mining", "MOCHI", None)
      ),
      alliancesById = Map(3001L -> EsiAlliance(3001L, "The Prickle", "PRKL")),
      routeByEnds = Map((4000L, 5000L) -> List(4001L, 4002L)),
      systemsById = Map(
        4001L -> EsiSolarSystem(4001L, "Jita", 5001L),
        4002L -> EsiSolarSystem(4002L, "Amarr", 5002L)
      ),
      constellationsById = Map(
        5001L -> EsiConstellation(5001L, "Kimotoro", 6001L),
        5002L -> EsiConstellation(5002L, "Domain", 6002L)
      ),
      namesById = Map(6001L -> "The Forge", 6002L -> "Domain")
    )

  private def run(esi: FakeEsi, query: String): IO[GraphQLResponse] = {
    val torpor = EsiGraphQL.torpor(esi)
    val schema = EsiGraphQL.schema(esi)
    torpor.query(schema, query) match {
      case Left(error) => IO.raiseError(new RuntimeException(error.render.mkString("; ")))
      case Right(io)   => io
    }
  }

  test("lookup by id") {
    val esi = fake()
    run(esi, "{ character(id: 1001) { id name } }").map { response =>
      assertEquals(response.data.hcursor.downField("character").get[String]("name").toOption, Some("Rex"))
      assertEquals(esi.characterCalls.get, 1)
      assertEquals(esi.nameLookups.get, 0)
    }
  }

  test("lookup by name resolves the id first") {
    val esi = fake()
    run(esi, """{ character(name: "Rex") { name } }""").map { response =>
      assertEquals(response.data.hcursor.downField("character").get[String]("name").toOption, Some("Rex"))
      assertEquals(esi.nameLookups.get, 1)
      assertEquals(esi.characterCalls.get, 1)
    }
  }

  test("an unknown name is null, not an error") {
    val esi = fake()
    run(esi, """{ character(name: "Nobody") { name } }""").map { response =>
      assert(response.data.hcursor.downField("character").focus.contains(Json.Null))
      assert(response.errors.isEmpty)
    }
  }

  test("the corporation is only fetched when selected") {
    val esi = fake()
    run(esi, "{ character(id: 1001) { name } }").map { _ =>
      assertEquals(esi.corporationCalls.get, 0, "unselected fields must not cost a call")
    }
  }

  test("selecting the corporation batches one call") {
    val esi = fake()
    run(esi, "{ character(id: 1001) { name corporation { name ticker } } }").map { response =>
      val corporation = response.data.hcursor.downField("character").downField("corporation")
      assertEquals(corporation.get[String]("name").toOption, Some("Hedgerow Holdings"))
      assertEquals(corporation.get[String]("ticker").toOption, Some("HEDGE"))
      assertEquals(esi.corporationCalls.get, 1)
      assertEquals(esi.allianceCalls.get, 0, "the alliance is a separate, unselected node")
    }
  }

  test("the alliance hydrates from the corporation when selected") {
    val esi = fake()
    run(esi, "{ character(id: 1001) { corporation { name alliance { name ticker } } } }").map { response =>
      val alliance = response.data.hcursor.downField("character").downField("corporation").downField("alliance")
      assertEquals(alliance.get[String]("name").toOption, Some("The Prickle"))
      assertEquals(alliance.get[String]("ticker").toOption, Some("PRKL"))
      assertEquals(esi.corporationCalls.get, 1)
      assertEquals(esi.allianceCalls.get, 1)
    }
  }

  test("a corporation with no alliance yields null and no error") {
    val esi = fake()
    run(esi, "{ character(id: 1002) { corporation { name alliance { name } } } }").map { response =>
      assertEquals(
        response.data.hcursor.downField("character").downField("corporation").get[String]("name").toOption,
        Some("Mochi Mining")
      )
      assert(
        response.data.hcursor
          .downField("character")
          .downField("corporation")
          .downField("alliance")
          .focus
          .contains(Json.Null)
      )
      assert(response.errors.isEmpty, response.errors.toList.map(_.message).mkString("; "))
    }
  }

  test("esi paths keep their slashes and responses get their id back") {
    var seen: Option[Uri] = None
    // a real ESI body: the id is in the path, not the JSON, and fields are snake_case
    val body              =
      """{"constellation_id":20000020,"name":"Jita","security_status":0.9459,"star_id":40009076,"system_id":30000142}"""
    val app               = HttpApp[IO] { request =>
      IO { seen = Some(request.uri); Response[IO](Status.Ok).withEntity(body) }
    }
    val esi               = EsiApi.http[IO](Client.fromHttpApp(app), EsiConfig(userAgent = "test"))
    esi.systems(NonEmptyList.one(30000142L)).map { found =>
      assertEquals(
        seen.map(_.renderString),
        Some("https://esi.evetech.net/latest/universe/systems/30000142")
      )
      assertEquals(found, Map(30000142L -> EsiSolarSystem(30000142L, "Jita", 20000020L)))
    }
  }

  test("characters are two batched requests, whatever the number of ids") {
    val paths = scala.collection.mutable.ListBuffer.empty[String]
    val app   = HttpApp[IO] { request =>
      IO {
        paths += request.uri.path.renderString
        request.uri.path.renderString match {
          case "/latest/characters/affiliation" =>
            Response[IO](Status.Ok).withEntity(
              """[{"character_id":443630591,"corporation_id":1000167},{"character_id":443630592,"corporation_id":1000167}]"""
            )
          case "/latest/universe/names"         =>
            Response[IO](Status.Ok).withEntity(
              """[{"category":"character","id":443630591,"name":"The Mittani"},{"category":"character","id":443630592,"name":"Someone"}]"""
            )
          case other                            => Response[IO](Status.NotFound).withEntity(s"unexpected $other")
        }
      }
    }
    val esi   = EsiApi.http[IO](Client.fromHttpApp(app), EsiConfig(userAgent = "test"))
    esi.characters(NonEmptyList.of(443630591L, 443630592L)).map { found =>
      assertEquals(paths.toList.sorted, List("/latest/characters/affiliation", "/latest/universe/names"))
      assertEquals(found(443630591L), EsiCharacter(443630591L, "The Mittani", 1000167L))
      assertEquals(found(443630592L), EsiCharacter(443630592L, "Someone", 1000167L))
    }
  }

  test("a route passes its flag and reads ESI's 404 as no route") {
    var seen: Option[Uri] = None
    val app               = HttpApp[IO] { request =>
      IO { seen = Some(request.uri) } *> IO.pure(Response[IO](Status.NotFound).withEntity("no route"))
    }
    val esi               = EsiApi.http[IO](Client.fromHttpApp(app), EsiConfig(userAgent = "test"))
    esi.route(30000142L, 30002187L, Some("secure")).map { route =>
      assertEquals(route, None)
      assertEquals(
        seen.map(_.renderString),
        Some("https://esi.evetech.net/latest/route/30000142/30002187?flag=secure")
      )
    }
  }

  test("a route hydrates every level in one call, except the systems ESI cannot batch") {
    val esi = fake()
    run(esi, "{ route(origin: 4000, destination: 5000) { name constellation { name region { name } } } }").map {
      response =>
        val route = response.data.hcursor.downField("route")
        assertEquals(
          route.values.map(_.flatMap(_.hcursor.get[String]("name").toOption)).map(_.toList),
          Some(List("Jita", "Amarr"))
        )
        assertEquals(
          route.values
            .map(_.flatMap(_.hcursor.downField("constellation").downField("region").get[String]("name").toOption))
            .map(_.toList),
          Some(List("The Forge", "Domain"))
        )
        assertEquals(esi.routeCalls.get, 1)
        assertEquals(esi.systemKeys.get, 2, "ESI has no batch system endpoint")
        assertEquals(esi.constellationKeys.get, 2, "two systems, two constellations, one call")
        assertEquals(esi.nameKeys.get, 2, "two regions, one names call")
    }
  }

  test("no route is null, not an error") {
    run(fake(), "{ route(origin: 1, destination: 2) { name } }").map { response =>
      assert(response.data.hcursor.downField("route").focus.contains(Json.Null))
      assert(response.errors.isEmpty)
    }
  }

  test("explain prints the plan without running a resolver") {
    val esi     = fake()
    val schema  = EsiGraphQL.schema(esi)
    val app     = ExplainRoute.routes(schema).orNotFound
    val request = org.http4s
      .Request[IO](Method.POST, Uri.unsafeFromString("/explain"))
      .withEntity(
        Json.obj(
          "query" -> Json.fromString(
            "{ route(origin: 4000, destination: 5000) { name constellation { region { name } } } }"
          )
        )
      )

    app.run(request).flatMap(_.as[String]).map { body =>
      assert(body.contains("flowchart LR"), body)
      assert(body.contains("routes: Query.route"), body)
      assert(body.contains("universe: Constellation.region"), body)
      assertEquals(esi.routeCalls.get, 0, "explaining must not execute")
    }
  }

  test("a list of characters costs one call per level, not one per row") {
    val esi = fake()
    run(esi, "{ characters(ids: [1001, 1002, 1003]) { name corporation { name } } }").map { response =>
      val names =
        response.data.hcursor.downField("characters").values.map(_.flatMap(_.hcursor.get[String]("name").toOption))
      assertEquals(names.map(_.toList).getOrElse(Nil), List("Rex", "Mochi", "Biscuit"))

      assertEquals(esi.characterCalls.get, 1, "three ids, one characters call")
      assertEquals(esi.corporationCalls.get, 1, "three characters, one corporations call")
      assertEquals(esi.corporationKeys.get, 2, "two distinct corporations, so two keys")
    }
  }

  test("source order does not matter: definitions merge before any extension is applied") {
    val esi      = fake()
    // `universe` extends types `characters` defines, but it is listed first here
    val reversed =
      Torpor[IO](EsiGraphQL.routesSource(esi), EsiGraphQL.universeSource(esi), EsiGraphQL.charactersSource(esi))
    val schema   = reversed.schema.fold(
      errors => fail(errors.toChain.toList.map(_.render).mkString("; ")),
      identity
    )

    assertEquals(reversed.checkCoverage(schema), Right(()))
    reversed.query(schema, "{ character(id: 1001) { corporation { alliance { name } } } }") match {
      case Left(error) => fail(error.render.mkString("; "))
      case Right(io)   =>
        io.map { response =>
          assertEquals(
            response.data.hcursor
              .downField("character")
              .downField("corporation")
              .downField("alliance")
              .get[String]("name")
              .toOption,
            Some("The Prickle")
          )
        }
    }
  }

  test("every SDL field the example declares has a resolver") {
    val esi    = fake()
    val torpor = EsiGraphQL.torpor(esi)
    assertEquals(torpor.checkCoverage(EsiGraphQL.schema(esi)), Right(()))
  }
}
