package net.andimiller.torpor.source

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.{Decoder, Encoder, Json}
import net.andimiller.torpor.core.{Argument, FieldCoordinate, HookError, Hooks}
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

/** A consumer needs only the fields it reads, so another context can read the same parent more narrowly. */
final case class HasOwner(ownerId: String) derives Decoder

/** A producer whose JSON leaves out `ownerId`, so a consumer cannot decode it back. */
final case class PrivatePet(id: String, name: String, ownerId: String) derives Decoder

object PrivatePet {
  given Encoder[PrivatePet] = Encoder.forProduct2("id", "name")(pet => (pet.id, pet.name))
}

final case class LinkedAdoption(pet: LinkedPet) derives Encoder.AsObject

/** A key type with no codec: the executor matches keys by Scala equality. */
final case class OwnerKey(value: String)

/** A producer whose shape has no `ownerId`, and one where it has the wrong type. */
final case class PetSummary(id: String, name: String) derives Encoder.AsObject
final case class PetWithNumericOwner(id: String, name: String, ownerId: Long) derives Encoder.AsObject

final class LinksSuite extends munit.CatsEffectSuite {
  import LinkedPets.*

  private def query(torpor: Torpor[IO], query: String): IO[String] =
    for {
      schema   <- IO.fromEither(torpor.schema.left.map(e => new RuntimeException(e.toString)))
      response <- torpor.query(schema, query) match {
                    case Left(error) => IO.raiseError(new RuntimeException(error.render.mkString("; ")))
                    case Right(run)  => run
                  }
    } yield response.toJson.noSpaces

  test("sources whose links line up assemble and serve a query") {
    query(Torpor[IO](petsSource, ownersSource), """{ pet(id: "p1") { name owner { name } } }""")
      .map(json => assertEquals(json, """{"data":{"pet":{"name":"Rex","owner":{"name":"Ada"}}}}"""))
  }

  test("a keyed field reads the value its producer made, not that value's JSON") {
    val privatePets   = Source[IO]("pets", pets)(
      resolver.root[IO](pets.Query.pet) { args => IO.pure(Option(PrivatePet(args.id, "Rex", "o1"))) }
    )
    val privateOwners = Source[IO]("owners", owners)(
      resolver
        .field[IO](owners.Pet.owner)
        .key((pet: PrivatePet) => pet.ownerId)
        .batch((_, ids: NonEmptyList[String]) => IO.pure(ids.toList.map(id => id -> LinkedOwner(id, "Ada")).toMap))
    )

    query(Torpor[IO](privatePets, privateOwners), """{ pet(id: "p1") { name owner { name } } }""")
      .map(json => assertEquals(json, """{"data":{"pet":{"name":"Rex","owner":{"name":"Ada"}}}}"""))
  }

  /** Rewrites what `Query.pet` produces, as an authorization hook may. */
  private final class RewritePet(rewrite: Json => Json) extends Hooks[IO] {
    override def authorize(
        coordinate: FieldCoordinate,
        args: List[Argument],
        values: Map[String, Json]
    ): IO[Map[String, Either[HookError, Json]]] =
      IO.pure(if (coordinate == pets.Query.pet) values.map { case (key, value) => key -> Right(rewrite(value)) }
      else Map.empty)
  }

  test("a hook that changes a produced value makes keyed fields read the new JSON") {
    val torpor =
      Torpor[IO](petsSource, ownersSource).withHooks(RewritePet(_.mapObject(_.add("ownerId", Json.fromString("o9")))))
    query(torpor, """{ pet(id: "p1") { owner { id } } }""")
      .map(json => assertEquals(json, """{"data":{"pet":{"owner":{"id":"o9"}}}}"""))
  }

  test("a hook that keeps a produced value keeps its Scala value") {
    // `PrivatePet`'s JSON has no `ownerId`, so only the produced value can give the key
    val privatePets   = Source[IO]("pets", pets)(
      resolver.root[IO](pets.Query.pet) { args => IO.pure(Option(PrivatePet(args.id, "Rex", "o1"))) }
    )
    val privateOwners = Source[IO]("owners", owners)(
      resolver
        .field[IO](owners.Pet.owner)
        .key((pet: PrivatePet) => pet.ownerId)
        .batch((_, ids: NonEmptyList[String]) => IO.pure(ids.toList.map(id => id -> LinkedOwner(id, "Ada")).toMap))
    )

    query(
      Torpor[IO](privatePets, privateOwners).withHooks(RewritePet(identity)),
      """{ pet(id: "p1") { owner { id } } }"""
    )
      .map(json => assertEquals(json, """{"data":{"pet":{"owner":{"id":"o1"}}}}"""))
  }

  test("a narrower consumer reads its fields from the produced value") {
    // a single resolver needs no tuple
    val narrow = Source[IO]("owners", owners)(
      resolver
        .field[IO](owners.Pet.owner)
        .key((parent: HasOwner) => parent.ownerId)
        .batch((_, ids: NonEmptyList[String]) =>
          IO.pure(ids.toList.map(id => id -> LinkedOwner(id, s"owner of $id")).toMap)
        )
    )

    query(Torpor[IO](petsSource, narrow), """{ pet(id: "p1") { owner { name } } }""")
      .map(json => assertEquals(json, """{"data":{"pet":{"owner":{"name":"owner of o1"}}}}"""))
  }

  test("a batch key needs no codec") {
    val byOwnerId = Source[IO]("owners", owners)(
      resolver
        .field[IO](owners.Pet.owner)
        .key((pet: LinkedPet) => OwnerKey(pet.ownerId))
        .batch((_, ids: NonEmptyList[OwnerKey]) =>
          IO.pure(ids.toList.map(id => id -> LinkedOwner(id.value, "Ada")).toMap)
        )
    )

    query(Torpor[IO](petsSource, byOwnerId), """{ pet(id: "p1") { owner { id } } }""")
      .map(json => assertEquals(json, """{"data":{"pet":{"owner":{"id":"o1"}}}}"""))
  }

  test("a parent nested inside another value is decoded from its JSON") {
    val adoptions       = schema"""type Adoption { pet: Pet! }
                            |extend type Query { adoption: Adoption }
                            |"""
    // `Adoption.pet` is an inline read, so no resolver produces the pet itself: `Pet.owner` decodes its JSON
    val adoptionsSource = Source[IO]("adoptions", adoptions)(
      resolver.root[IO](adoptions.Query.adoption) { _ =>
        IO.pure(Option(LinkedAdoption(LinkedPet("p2", "Mochi", "o2"))))
      }
    )

    query(Torpor[IO](petsSource, ownersSource, adoptionsSource), """{ adoption { pet { owner { id } } } }""")
      .map(json => assertEquals(json, """{"data":{"adoption":{"pet":{"owner":{"id":"o2"}}}}}"""))
  }

  test("a consumer that is neither a case class nor a supertype of the producer does not compile") {
    val errors = compileErrors("""
      val opaque = Source[IO]("owners", owners)(
        resolver
          .field[IO](owners.Pet.owner)
          .key((parent: String) => parent)
          .batch((_, ids: NonEmptyList[String]) => IO.pure(ids.toList.map(id => id -> LinkedOwner(id, "Ada")).toMap))
      )
      Torpor[IO](petsSource, opaque)
    """)
    assert(
      errors.contains(
        "Pet.owner reads its parent as a type that is neither a case class nor a supertype of what Query.pet produces as Pet"
      ),
      errors
    )
  }

  test("a producer without the field a consumer reads does not compile") {
    val errors = compileErrors("""
      val summaries = Source[IO]("pets", pets)(
        resolver.root[IO](pets.Query.pet) { args => IO.pure(Option(PetSummary(args.id, "Rex"))) }
      )
      Torpor[IO](summaries, ownersSource)
    """)
    assert(
      errors.contains("Pet.owner reads `ownerId` from its parent, but Query.pet produces Pet without it"),
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
    assert(
      errors.contains(
        "Pet.owner reads `ownerId` from its parent, but Query.pet produces Pet with `ownerId` of another type"
      ),
      errors
    )
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
