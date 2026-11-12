package net.andimiller.torpor.source

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.{Decoder, Encoder}
import net.andimiller.torpor.macros.schema

/** The pets source produces `Pet` values as `LinkedPet`; the owners source hydrates `Pet.owner` from `ownerId`. */
object LinkedPets {
  val pets = schema"""type Pet { id: ID! name: String! }
                     |extend type Query { pet(id: ID!): Pet }
                     |"""

  val owners = schema"""type Owner { id: ID! name: String! }
                       |extend type Pet { owner: Owner! }
                       |"""

  val petsSource = Source[IO]("pets", pets)(
    resolver.root[IO](pets.Query.pet) { args => IO.pure(Option(LinkedPet(args.id, "Rex", "o1"))) }
  )

  def ownersSource = Source[IO]("owners", owners)(
    resolver
      .field[IO](owners.Pet.owner)
      .key((pet: LinkedPet) => pet.ownerId)
      .batch((_, ids: NonEmptyList[String]) => IO.pure(ids.toList.map(id => id -> LinkedOwner(id, "Ada")).toMap))
  )
}

final case class LinkedPet(id: String, name: String, ownerId: String) derives Encoder.AsObject, Decoder
final case class LinkedOwner(id: String, name: String) derives Encoder.AsObject

/** A consumer needs only the fields it reads, so another context can decode the same parent more narrowly. */
final case class HasOwner(ownerId: String) derives Decoder

/** A producer whose shape has no `ownerId`, and one where it has the wrong type. */
final case class PetSummary(id: String, name: String) derives Encoder.AsObject
final case class PetWithNumericOwner(id: String, name: String, ownerId: Long) derives Encoder.AsObject

final class LinkCheckSuite extends munit.CatsEffectSuite {
  import LinkedPets.*

  test("sources whose links line up assemble and serve a query") {
    val torpor = Torpor[IO](petsSource, ownersSource)
    val result = for {
      schema   <- IO.fromEither(torpor.schema.left.map(e => new RuntimeException(e.toString)))
      response <- torpor.query(schema, """{ pet(id: "p1") { name owner { name } } }""") match {
                    case Left(error) => IO.raiseError(new RuntimeException(error.render.mkString("; ")))
                    case Right(run)  => run
                  }
    } yield response.toJson.noSpaces

    result.map(json => assertEquals(json, """{"data":{"pet":{"name":"Rex","owner":{"name":"Ada"}}}}"""))
  }

  test("a consumer that decodes a narrower parent still links") {
    // a single resolver needs no tuple
    val narrow = Source[IO]("owners", owners)(
      resolver
        .field[IO](owners.Pet.owner)
        .key((parent: HasOwner) => parent.ownerId)
        .batch((_, ids: NonEmptyList[String]) => IO.pure(ids.toList.map(id => id -> LinkedOwner(id, "Ada")).toMap))
    )

    Torpor[IO](petsSource, narrow)
  }

  test("a producer without the field a consumer reads does not compile") {
    val errors = compileErrors("""
      val summaries = Source[IO]("pets", pets)(
        resolver.root[IO](pets.Query.pet) { args => IO.pure(Option(PetSummary(args.id, "Rex"))) }
      )
      Torpor[IO](summaries, ownersSource)
    """)
    assert(errors.contains("sources do not link"), errors)
    assert(
      errors.contains(
        "Pet.owner (owners) reads `ownerId: String` from its parent, but Query.pet (pets) produces Pet as PetSummary, which has no `ownerId`"
      ),
      errors
    )
  }

  test("a producer whose field has another type does not compile") {
    val errors = compileErrors("""
      val numeric = Source[IO]("pets", pets)(
        resolver.root[IO](pets.Query.pet) { args => IO.pure(Option(PetWithNumericOwner(args.id, "Rex", 1L))) }
      )
      Torpor[IO](numeric, ownersSource)
    """)
    assert(errors.contains("where `ownerId` is Long"), errors)
  }

  test("something that is not a resolver does not compile") {
    val errors = compileErrors("""Source[IO]("owners", owners)("not a resolver", 1)""")
    assert(errors.contains("Match type reduction failed"), errors)
  }

  test("a source with a resolver missing does not compile") {
    val errors = compileErrors("""Torpor[IO](petsSource, Source[IO]("owners", owners))""")
    assert(errors.contains("source 'owners' has no resolver for Pet.owner"), errors)
  }

  test("a source takes its resolvers once") {
    val errors = compileErrors("""
      petsSource(resolver.root[IO](pets.Query.pet) { args => IO.pure(Option.empty[LinkedPet]) })
    """)
    assert(errors.contains("EmptyTuple"), errors)
  }
}
