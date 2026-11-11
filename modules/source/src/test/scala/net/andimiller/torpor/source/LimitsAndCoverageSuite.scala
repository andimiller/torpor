package net.andimiller.torpor.source

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.{Decoder, Encoder}
import net.andimiller.torpor.core.{Plan, Value}
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.schema.{Executable, Schema}

final case class CoveragePet(id: String, ownerId: String) derives Decoder, Encoder.AsObject

class LimitsAndCoverageSuite extends munit.FunSuite {

  private val petsSdl =
    schema"""type Pet { id: ID! name: String! }
      |extend type Query { pets(first: Int = 20): [Pet!]! }
      |"""

  private val ownersSdl =
    schema"""type Owner { id: ID! name: String! }
      |extend type Pet { owner: Owner! }
      |"""

  private val pet = CoveragePet("p1", "o1")

  private val pets = Source[IO]("pets", petsSdl.text)(resolver.root[IO](petsSdl.Query.pets) { _ => IO(List(pet)) })

  private val owners = Source[IO]("owners", ownersSdl.text)(
    resolver
      .field[IO](ownersSdl.Pet.owner)
      .key((p: CoveragePet) => p.ownerId)
      .batch((keys: NonEmptyList[String]) => IO(keys.toList.map(k => k -> "Ada").toMap))
  )

  /** Declares `Pet.owner` but serves nothing, which is what the startup check must catch. */
  private val unservedOwners = Source[IO]("owners", ownersSdl.text)

  /** `Pet.name` is declared in the type definition, so nothing should resolve it; registering one is a mistake. */
  private val unused = Source[IO]("unused", "")(
    resolver
      .field[IO](petsSdl.Pet.name)
      .key((p: CoveragePet) => p.id)
      .batch((keys: NonEmptyList[String]) => IO(keys.toList.map(k => k -> "x").toMap))
  )

  private def schemaOf(torpor: Torpor[IO]): Schema =
    torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

  private def planWith(limits: Planner.Limits, query: String): Either[String, Plan] = {
    val schema = schemaOf(Torpor[IO](pets, owners))
    Executable.validate(schema, query) match {
      case Left(errors)     => Left(errors.toChain.toList.map(_.render).mkString("\n"))
      case Right(validated) =>
        Planner
          .build(schema, validated, Map.empty[String, Value], limits)
          .left
          .map(_.toChain.toList.map(_.render).mkString("\n"))
    }
  }

  test("a query within the depth limit plans") {
    assert(planWith(Planner.Limits(maxDepth = 3), "{ pets { name owner { name } } }").isRight)
  }

  test("a query deeper than maxDepth is rejected") {
    planWith(Planner.Limits(maxDepth = 2), "{ pets { name owner { name } } }") match {
      case Right(_)    => fail("expected a depth error")
      case Left(error) =>
        assert(error.contains("query depth 3 exceeds maxDepth 2"), error)
        assert(error.contains("limit exceeded"), error)
    }
  }

  test("depth counts inline fields, not just batch calls") {
    // pets(name) -> owner(name) is two levels of batch calls, but four levels of response nesting
    val schema = schemaOf(Torpor[IO](pets, owners))
    val plan   = Executable.validate(schema, "{ pets { name owner { name } } }") match {
      case Left(errors)     => fail(errors.toChain.toList.map(_.render).mkString("\n"))
      case Right(validated) =>
        Planner
          .build(schema, validated)
          .fold(
            errors => fail(errors.toChain.toList.map(_.render).mkString("\n")),
            identity
          )
    }

    assertEquals(plan.levels, 2)
    assertEquals(plan.depth, 3)
  }

  test("a query with too many fields is rejected") {
    // pets, name, owner, name: the root selection counts once, and the two nodes are each counted once
    planWith(Planner.Limits.none, "{ pets { name owner { name } } }") match {
      case Left(error) => fail(error)
      case Right(plan) => assertEquals(plan.fieldCount, 4)
    }

    planWith(Planner.Limits(maxFields = 3), "{ pets { name owner { name } } }") match {
      case Right(_)    => fail("expected a field-count error")
      case Left(error) => assert(error.contains("4 fields"), error)
    }
  }

  test("a fully served schema passes the startup coverage check") {
    val torpor = Torpor[IO](pets, owners)
    assertEquals(torpor.checkCoverage(schemaOf(torpor)), Right(()))
  }

  test("an SDL extension field with no resolver fails startup and names the field") {
    val torpor = Torpor[IO](pets, unservedOwners)

    torpor.checkCoverage(schemaOf(torpor)) match {
      case Right(_)     => fail("expected a coverage error for Pet.owner")
      case Left(errors) =>
        val rendered = errors.toChain.toList.map(_.render)
        assertEquals(rendered.size, 1)
        assert(rendered.head.contains("Pet.owner"), rendered.head)
        assert(rendered.head.contains("owners"), rendered.head)
    }
  }

  test("a resolver for a field that needs none is reported") {
    val torpor = Torpor[IO](pets, owners, unused)

    torpor.checkCoverage(schemaOf(torpor)) match {
      case Right(_)     => fail("expected a coverage error for Pet.name")
      case Left(errors) =>
        assert(
          errors.toChain.toList.exists(_.render.contains("Pet.name")),
          errors.toChain.toList.map(_.render).mkString("\n")
        )
    }
  }

  test("startup merges and checks coverage, raising every problem at once") {
    import cats.effect.unsafe.implicits.global

    assert(Torpor[IO](pets, owners).startup.attempt.unsafeRunSync().isRight)

    Torpor[IO](pets, unservedOwners).startup.attempt.unsafeRunSync() match {
      case Right(_)    => fail("expected startup to refuse an unserved field")
      case Left(error) =>
        assert(error.isInstanceOf[TorporStartupException], error.toString)
        assert(error.getMessage.contains("Pet.owner"), error.getMessage)
    }
  }

  test("the request path applies the limits") {
    val torpor = Torpor[IO](pets, owners).withLimits(Planner.Limits(maxDepth = 1))
    val schema = schemaOf(torpor)

    torpor.query(schema, "{ pets { name owner { name } } }") match {
      case Right(_)    => fail("expected the limit to reject the query")
      case Left(error) =>
        assert(error.render.exists(_.contains("exceeds maxDepth")), error.render.mkString("\n"))
    }
  }
}
