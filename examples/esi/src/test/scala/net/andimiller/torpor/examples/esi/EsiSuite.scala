package net.andimiller.torpor.examples.esi

import cats.data.NonEmptyList
import cats.effect.IO
import net.andimiller.torpor.examples.esi.client.*
import net.andimiller.torpor.core.PathSegment
import net.andimiller.torpor.source.{Planner, RunOutcome, Torpor}
import net.andimiller.torpor.schema.Executable
import org.http4s.{HttpApp, Response, Status, Uri}
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
      alliancesById: Map[Long, EsiAlliance]
  ) extends EsiApi[IO] {
    val characterCalls   = new AtomicInteger(0)
    val nameLookups      = new AtomicInteger(0)
    val corporationCalls = new AtomicInteger(0)
    val allianceCalls    = new AtomicInteger(0)
    val corporationKeys  = new AtomicInteger(0)

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

    def alliances(ids: NonEmptyList[Long]): IO[Map[Long, EsiAlliance]] =
      IO { allianceCalls.incrementAndGet(); ids.toList.flatMap(id => alliancesById.get(id).map(id -> _)).toMap }
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
      alliancesById = Map(3001L -> EsiAlliance(3001L, "The Prickle", "PRKL"))
    )

  private def run(esi: FakeEsi, query: String): IO[net.andimiller.torpor.core.Response] = {
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
      assert(response.data.hcursor.downField("character").focus.contains(io.circe.Json.Null))
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
          .contains(io.circe.Json.Null)
      )
      assert(response.errors.isEmpty, response.errors.toList.map(_.message).mkString("; "))
    }
  }

  test("esi paths keep their slashes and responses get their id back") {
    var seen: Option[Uri] = None
    // a real ESI body: no id in the JSON, and snake_case fields
    val body              =
      """{"name":"The Mittani","corporation_id":1000167,"birthday":"2005-06-08T18:24:00Z","bloodline_id":1,"gender":"male","race_id":1,"security_status":-1.0}"""
    val app               = HttpApp[IO] { request =>
      IO { seen = Some(request.uri); Response[IO](Status.Ok).withEntity(body) }
    }
    val esi               = EsiApi.http[IO](Client.fromHttpApp(app), EsiConfig(userAgent = "test"))
    esi.characters(NonEmptyList.one(443630591L)).map { found =>
      assertEquals(
        seen.map(_.renderString),
        Some("https://esi.evetech.net/latest/characters/443630591")
      )
      assertEquals(found, Map(443630591L -> EsiCharacter(443630591L, "The Mittani", 1000167L)))
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
    val reversed = Torpor[IO](EsiGraphQL.sources(esi).reverse: _*)
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
