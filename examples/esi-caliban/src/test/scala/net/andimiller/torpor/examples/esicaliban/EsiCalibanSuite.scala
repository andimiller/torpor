package net.andimiller.torpor.examples.esicaliban

import net.andimiller.torpor.examples.esi.client.*
import cats.data.NonEmptyList
import zio.*

import java.util.concurrent.atomic.AtomicInteger

/** The same checks as the torpor suite, minus the ones about batching (Caliban has no planning step to test): that a
  * query resolves from ESI rows, and that an unselected field costs no ESI call.
  */
final class EsiCalibanSuite extends munit.FunSuite {

  private val characterCalls   = new AtomicInteger(0)
  private val corporationCalls = new AtomicInteger(0)

  private val charactersById   = Map(
    1001L -> EsiCharacter(1001L, "Rex", 2001L),
    1002L -> EsiCharacter(1002L, "Mochi", 2002L),
    1003L -> EsiCharacter(1003L, "Biscuit", 2001L)
  )
  private val corporationsById = Map(
    2001L -> EsiCorporation(2001L, "Hedgerow Holdings", "HEDGE", Some(3001L)),
    2002L -> EsiCorporation(2002L, "Mochi Mining", "MOCHI", None)
  )
  private val alliancesById    = Map(3001L -> EsiAlliance(3001L, "The Prickle", "PRKL"))

  private val esi: EsiApi[Task] = new EsiApi[Task] {
    def characterIdByName(name: String): Task[Option[Long]] =
      ZIO.succeed(Option(1001L).filter(_ => name == "Rex"))

    def characters(ids: NonEmptyList[Long]): Task[Map[Long, EsiCharacter]] =
      ZIO.succeed {
        characterCalls.incrementAndGet()
        ids.toList.flatMap(id => charactersById.get(id).map(id -> _)).toMap
      }

    def corporations(ids: NonEmptyList[Long]): Task[Map[Long, EsiCorporation]] =
      ZIO.succeed {
        corporationCalls.incrementAndGet()
        ids.toList.flatMap(id => corporationsById.get(id).map(id -> _)).toMap
      }

    def alliances(ids: NonEmptyList[Long]): Task[Map[Long, EsiAlliance]] =
      ZIO.succeed(ids.toList.flatMap(id => alliancesById.get(id).map(id -> _)).toMap)
  }

  // one suite instance for all tests, so counters are reset per test
  override def beforeEach(context: BeforeEach): Unit = {
    characterCalls.set(0)
    corporationCalls.set(0)
  }

  private def run(query: String): caliban.GraphQLResponse[caliban.CalibanError] =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(EsiCaliban.graphqlFor(esi).interpreter.flatMap(_.execute(query)))
        .getOrThrowFiberFailure()
    }

  test("resolves a character by id") {
    val response = run("{ character(id: 1001) { id name } }")
    assert(response.errors.isEmpty, response.errors.toString)
    assert(response.data.toString.contains("Rex"))
    assertEquals(characterCalls.get, 1)
  }

  test("an unselected corporation costs no ESI call") {
    run("{ character(id: 1001) { name } }")
    assertEquals(corporationCalls.get, 0)
  }

  test("the alliance hydrates only when selected") {
    val response = run("{ character(id: 1001) { corporation { name ticker alliance { name ticker } } } }")
    assert(response.errors.isEmpty, response.errors.toString)
    assert(response.data.toString.contains("HEDGE"))
    assert(response.data.toString.contains("PRKL"))
    assertEquals(corporationCalls.get, 1)
  }

  test("a list of characters costs one corporation call per row") {
    val response = run("{ characters(ids: [1001, 1002, 1003]) { name corporation { name } } }")
    assert(response.errors.isEmpty, response.errors.toString)
    assert(response.data.toString.contains("Mochi"), response.data.toString)
    assertEquals(characterCalls.get, 1, "three ids, one characters call")
    assertEquals(corporationCalls.get, 3, "Caliban resolves corporation inside each character, so one call per row")
  }

  test("the schema matches the torpor example, apart from id being Long rather than ID") {
    val sdl = EsiCaliban.graphqlFor(esi).render
    assert(sdl.contains("corporation: Corporation!"), sdl)
    assert(sdl.contains("alliance: Alliance\n"), sdl)
    assert(sdl.contains("character(id: Long, name: String): Character"), sdl)
  }
}
