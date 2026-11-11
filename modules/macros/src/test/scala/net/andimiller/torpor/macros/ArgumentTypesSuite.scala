package net.andimiller.torpor.macros

import io.circe.Json
import net.andimiller.torpor.core.{Arguments, FieldCoordinate, Scalar}

object BoundIds {
  given Scalar["ID", Long]()

  val pets =
    schema"""enum Species { DOG CAT }
            |input Range { from: Int! to: Int }
            |scalar Date
            |type Pet { id: ID! }
            |extend type Query {
            |  pets(ids: [ID!], species: Species, range: Range, born: Date, first: Int = 10, name: String!): [Pet!]!
            |}
            |"""

  type RangeSpec = (("from", Int), ("to", Option[Int]))

  type PetsSpec = (
      ("ids", Option[List[Long]]),
      ("species", Option[Arguments.OneOf[("DOG", "CAT")]]),
      ("range", Option[Arguments[RangeSpec]]),
      ("born", Option[Json]),
      ("first", Option[Int]),
      ("name", String)
  )

  type PetsArgs = Arguments[PetsSpec] {
    val ids: Option[List[Long]]
    val species: Option["DOG" | "CAT"]
    val range: Option[Arguments[RangeSpec] { val from: Int; val to: Option[Int] }]
    val born: Option[Json]
    val first: Option[Int]
    val name: String
  }
}

object UnboundIds {
  val pets = schema"""type Pet { id: ID! }
                     |extend type Query { pet(id: ID!): Pet }
                     |"""
}

class ArgumentTypesSuite extends munit.FunSuite {
  import BoundIds.PetsArgs

  private def decode(fields: (String, Json)*): Either[String, PetsArgs] =
    Arguments.decoder[PetsArgs].decodeJson(Json.obj(fields*)).left.map(_.getMessage)

  test("arguments are typed from the SDL: nullable, lists, enums, input objects, defaults, bound and unbound scalars") {
    val typed: FieldCoordinate.Of["Query", "pets", PetsArgs, "Pet"] = BoundIds.pets.Query.pets
    assertEquals(typed, FieldCoordinate("Query", "pets"))
  }

  test("ID is a String when no Scalar binds it") {
    val typed: FieldCoordinate.Of["Query", "pet", Arguments[("id", String) *: EmptyTuple] { val id: String }, "Pet"] =
      UnboundIds.pets.Query.pet
    assertEquals(typed, FieldCoordinate("Query", "pet"))
  }

  test("a field without arguments has an empty Arguments") {
    val typed: FieldCoordinate.Of["Pet", "id", Arguments[EmptyTuple], "ID"] = BoundIds.pets.Pet.id
    assertEquals(typed, FieldCoordinate("Pet", "id"))
  }

  test("the derived decoder reads every member, leaving absent nullable arguments as None") {
    val args = decode(
      "ids"     -> Json.arr(Json.fromInt(1), Json.fromInt(2)),
      "species" -> Json.fromString("CAT"),
      "range"   -> Json.obj("from" -> Json.fromInt(1)),
      "name"    -> Json.fromString("Rex")
    ).fold(fail(_), identity)
    assertEquals(args.ids, Some(List(1L, 2L)))
    assertEquals[Option[String], Option[String]](args.species, Some("CAT"))
    assertEquals(args.range.map(r => (r.from, r.to)), Some((1, None)))
    assertEquals(args.born, None)
    assertEquals(args.first, None)
    assertEquals(args.name, "Rex")
  }

  test("an enum value the SDL does not declare is rejected") {
    assert(decode("species" -> Json.fromString("FISH"), "name" -> Json.fromString("Rex")).isLeft)
  }

  test("a missing non-null argument is rejected") {
    assert(decode("species" -> Json.fromString("CAT")).isLeft)
  }

  test("an argument the SDL does not declare is a compile error") {
    assert(compileErrors("(??? : PetsArgs).nope").contains("nope"), compileErrors("(??? : PetsArgs).nope"))
  }
}
