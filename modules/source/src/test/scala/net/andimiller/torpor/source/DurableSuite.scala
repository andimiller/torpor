package net.andimiller.torpor.source

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json}
import net.andimiller.torpor.core.Step
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.schema.{Schema, SdlFragment}
import net.andimiller.torpor.snapshot.*

import java.util.concurrent.atomic.AtomicInteger

final case class DurableReport(species: String, adoptions: Int) derives Encoder.AsObject, Decoder
final case class DurablePet(id: String, name: String) derives Encoder.AsObject, Decoder

final class DurableSuite extends munit.FunSuite {
  import cats.effect.unsafe.implicits.global

  private val petsSdl    = schema"""type Pet { id: ID! name: String! }
                             |extend type Query { pets: [Pet!]! }
                             |"""
  private val reportsSdl = schema"""type AdoptionReport { species: Species! adoptions: Int! }
                             |enum Species { DOG CAT }
                             |extend type Query { adoptionReport(species: Species!): AdoptionReport! }
                             |"""

  private val query = """{ pets { name } adoptionReport(species: DOG) { adoptions } }"""

  private final class Calls {
    val pets   = new AtomicInteger(0)
    val resume = new AtomicInteger(0)
  }

  private def torpor(calls: Calls): Torpor[IO] =
    Torpor[IO](
      Source[IO]("pets", petsSdl)(
        resolver.root[IO](petsSdl.Query.pets) { _ =>
          IO {
            calls.pets.incrementAndGet()
            List(DurablePet("p1", "Rex"))
          }
        }
      ),
      Source[IO]("reports", reportsSdl)(
        resolver.suspending[IO](reportsSdl.Query.adoptionReport) { args =>
          IO(Step.suspend(s"job-${args.species}"))
        } { (handle: String, payload: Json) =>
          IO {
            calls.resume.incrementAndGet()
            val adoptions = payload.hcursor.get[Int]("adoptions").getOrElse(0)
            Step.complete(DurableReport("DOG", adoptions))
          }
        }
      )
    )

  private def schemaOf(torpor: Torpor[IO]): Schema =
    torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

  private final case class Suspended(runId: RunId, waiting: Map[String, Json], storedJson: Json, schema: Schema)

  /** Phase one: plan, start, suspend, persist. Returns only what a database would hold. */
  private def suspend(): Suspended = {
    val calls = new Calls
    val t     = torpor(calls)
    val s     = schemaOf(t)

    val program = for {
      store     <- SnapshotStore.inMemory[IO]
      durable   <- Durable[IO](t, store)
      outcome   <- durable.start(s, query)
      suspended <- IO.fromEither(
                     outcome.leftMap(error => new RuntimeException(error.render.mkString("; "))).flatMap {
                       case RunOutcome.Suspended(runId, waiting) => Right(runId -> waiting)
                       case RunOutcome.Finished(response)        =>
                         Left(new RuntimeException(s"expected a suspension, got ${response.data}"))
                     }
                   )
      stored    <- store.load(suspended._1).map(_.getOrElse(fail("run was not stored")))
    } yield Suspended(suspended._1, suspended._2, StoredRun.toJson(stored), s)

    program.unsafeRunSync()
  }

  /** Phase two: a fresh runtime, a fresh store, nothing but the encoded snapshot. */
  private def resume(
      state: Suspended,
      payloads: Map[String, Json]
  ): (Either[ResumeError, RunOutcome], Calls) = {
    val calls = new Calls
    val t     = torpor(calls)
    val s     = schemaOf(t)

    val stored =
      StoredRun.fromJson(state.storedJson).fold(error => fail(s"snapshot did not round trip: $error"), identity)

    val program = for {
      store   <- SnapshotStore.inMemory[IO]
      _       <- store.create(state.runId, stored)
      durable <- Durable[IO](t, store)
      outcome <- durable.resume(s, state.runId, payloads)
    } yield outcome

    val freshRuntime = IORuntime.builder().build()
    try (program.unsafeRunSync()(freshRuntime), calls)
    finally freshRuntime.shutdown()
  }

  test("a suspending resolver parks the run and hands back the handle") {
    val state            = suspend()
    assertEquals(state.waiting.size, 1)
    val (nodeId, handle) = state.waiting.head
    assert(handle.asString.contains("job-DOG"), handle.noSpaces)
    assert(nodeId.nonEmpty)
  }

  test("the run resumes in a fresh runtime from nothing but the stored snapshot") {
    val state            = suspend()
    val (outcome, calls) =
      resume(state, state.waiting.map { case (nodeId, _) => nodeId -> Json.obj("adoptions" -> Json.fromInt(7)) })

    outcome match {
      case Left(error)                             => fail(error.render)
      case Right(RunOutcome.Suspended(_, waiting)) => fail(s"expected completion, still waiting on $waiting")
      case Right(RunOutcome.Finished(response))    =>
        val cursor = response.data.hcursor
        // the resolved-before-suspension part came out of the snapshot, not from a second pets call
        assertEquals(calls.pets.get, 0)
        assertEquals(cursor.downField("pets").downArray.get[String]("name").toOption, Some("Rex"))
        assertEquals(cursor.downField("adoptionReport").get[Int]("adoptions").toOption, Some(7))
        assertEquals(calls.resume.get, 1)
        assert(response.errors.isEmpty)
    }
  }

  test("two webhooks racing for the same run: one wins, one conflicts, one downstream call") {
    val state    = suspend()
    val payloads = state.waiting.map { case (nodeId, _) => nodeId -> Json.obj("adoptions" -> Json.fromInt(3)) }

    val calls  = new Calls
    val t      = torpor(calls)
    val s      = schemaOf(t)
    val stored = StoredRun.fromJson(state.storedJson).fold(error => fail(error), identity)

    val program = for {
      store   <- SnapshotStore.inMemory[IO]
      _       <- store.create(state.runId, stored)
      durable <- Durable[IO](t, store)
      results <- (durable.resume(s, state.runId, payloads), durable.resume(s, state.runId, payloads)).parTupled
    } yield results

    val (left, right) = program.unsafeRunSync()
    val outcomes      = List(left, right)

    assertEquals(outcomes.count(_.isRight), 1)
    // The loser either lost the CAS (Conflict) or arrived after the winner deleted the run (NotFound). Both are a clean
    // loss; what matters is that it did not run downstream work.
    assertEquals(outcomes.count(_.isLeft), 1)
    val loser = outcomes.collectFirst { case Left(error) => error }.get
    assert(
      loser match {
        case ResumeError.Store(_: StoreError.Conflict) => true
        case ResumeError.Store(_: StoreError.NotFound) => true
        case _                                         => false
      },
      loser.render
    )
    assertEquals(calls.resume.get, 1, "only the winner may run downstream work")
  }

  test("a run planned against a different schema is refused") {
    val state = suspend()

    // a real schema change: the type gains a field, so the fingerprint moves
    val drifted = Schema
      .merge(
        List(
          SdlFragment(
            "pets",
            """type Pet { id: ID! name: String! nickname: String }
                                                            |extend type Query { pets: [Pet!]! }
                                                            |""".stripMargin
          ),
          SdlFragment("reports", reportsSdl.text)
        )
      )
      .fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

    val payloads = state.waiting.map { case (nodeId, _) => nodeId -> Json.obj("adoptions" -> Json.fromInt(1)) }
    val calls    = new Calls
    val t        = torpor(calls)
    val stored   = StoredRun.fromJson(state.storedJson).fold(error => fail(error), identity)

    val program = for {
      store   <- SnapshotStore.inMemory[IO]
      _       <- store.create(state.runId, stored)
      durable <- Durable[IO](t, store)
      outcome <- durable.resume(drifted, state.runId, payloads)
    } yield outcome

    program.unsafeRunSync() match {
      case Left(ResumeError.SchemaDrift(_, expected, actual)) =>
        assertNotEquals(expected, actual)
        assertEquals(calls.resume.get, 0)
      case other                                              => fail(s"expected a schema-drift refusal, got $other")
    }
  }
}
