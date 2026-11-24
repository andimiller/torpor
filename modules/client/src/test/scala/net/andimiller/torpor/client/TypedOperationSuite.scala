package net.andimiller.torpor.client

import io.circe.{Encoder, Json}

/** Value classes, written as the value they wrap. */
final case class Count(value: Int) extends AnyVal

object Count {
  given Encoder[Count] = Encoder.encodeInt.contramap(_.value)
}

final case class Label(value: String) extends AnyVal

object Label {
  given Encoder[Label] = Encoder.encodeString.contramap(_.value)
}

/** What compiles and what does not when an operation's types are written by the macro. */
final class TypedOperationSuite extends munit.FunSuite {

  private val library = Api.sdl(LibrarySchema.sdl)
  private val petshop = Api.sdl(PetshopSchema.sdl)

  private val shelves = library.operation(
    """
      query Shelves($filter: ShelfFilter, $first: Int = 10, $names: [String!]!) {
        shelves(filter: $filter, first: $first, names: $names) {
          name
          published
        }
      }
    """
  )

  test("an Api takes the schema as a literal") {
    val literal = Api.sdl("type Query { answer: Int! }").operation("{ answer }")

    val read: literal.Data => Int = _.answer
  }

  test("a schema that does not parse does not compile") {
    val errors = compileErrors("""Api.sdl("type Query { pets: }")""")

    assert(errors.contains("cannot parse SDL"), errors)
  }

  test("an operation keeps its document") {
    assert(shelves.document.contains("query Shelves"), shelves.document)
  }

  test("an operation keeps its name") {
    assertEquals(shelves.name, Some("Shelves"))
  }

  test("a variable is given by name") {
    val pet = petshop.operation("query P($id: ID!) { pet(id: $id) { name } }")

    assertEquals(pet(id = "1").variables.toMap, Map("id" -> Json.fromString("1")))
  }

  test("a variable with a default may be left out") {
    val pets = petshop.operation("query P($first: Int! = 2) { pets(first: $first) { name } }")

    assertEquals(pets().variables.toMap, Map.empty)
  }

  test("a nullable variable may be left out") {
    val pets = petshop.operation("query P($first: Int) { pets(first: $first) { name } }")

    assertEquals(pets().variables.toMap, Map.empty)
  }

  test("a union has a view per member when the document selects __typename") {
    val home =
      petshop.operation("{ home(pet: \"1\") { __typename ... on Owner { name } ... on Shelter { capacity } } }")

    val read: home.Data => Option[Int] = _.home.flatMap(_.asShelter).map(_.capacity)
  }

  test("a union has no view per member when the document does not select __typename") {
    val home   = petshop.operation("{ home(pet: \"1\") { ... on Owner { name } ... on Shelter { capacity } } }")
    val errors = compileErrors("""Option.empty[home.Data].flatMap(_.home).flatMap(_.asShelter)""")

    assert(errors.contains("asShelter"), errors)
  }

  test("an enum variable takes a literal of its values") {
    val pets = petshop.operation("query P($species: Species) { pets(species: $species) { name } }")

    assertEquals(pets(species = "DOG").variables.toMap.get("species"), Some(Json.fromString("DOG")))
  }

  test("an enum variable given a literal that is not one of its values does not compile") {
    val pets   = petshop.operation("query P($species: Species) { pets(species: $species) { name } }")
    val errors = compileErrors("""pets(species = "BIRD")""")

    assert(
      errors.contains("$species is the enum Species in the schema, but \"BIRD\" is not one of its values: DOG, CAT"),
      errors
    )
  }

  test("a value class is written as the value it wraps") {
    val pets = petshop.operation("query P($first: Int) { pets(first: $first) { name } }")

    assertEquals(pets(first = Count(2)).variables.toMap.get("first"), Some(Json.fromInt(2)))
  }

  test("a value class that wraps a value of the wrong type for a variable does not compile") {
    val pets   = petshop.operation("query P($first: Int) { pets(first: $first) { name } }")
    val errors = compileErrors("""pets(first = Label("two"))""")

    assert(
      errors.contains("$first is a Int in the schema, so it is written as BigInt or Int or Long, not String"),
      errors
    )
  }

  test("a field that cannot be null is read as its type") {
    val read: shelves.Data => List[String] = _.shelves.map(_.name)
  }

  test("a nullable field is read as an Option") {
    val pets = petshop.operation("{ pets { age } }")

    val read: pets.Data => List[Option[Int]] = _.pets.map(_.age)
  }

  test("a list is read as a List") {
    val tags = library.operation("{ shelves(names: []) { tags } }")

    val read: tags.Data => List[List[String]] = _.shelves.map(_.tags)
  }

  test("an object is read as a record of the fields that the operation selects") {
    val pets = petshop.operation("{ pets { owner { name } } }")

    val read: pets.Data => List[Option[String]] = _.pets.map(_.owner.map(_.name))
  }

  test("an alias is the name that the field is read by") {
    val fiction = library.operation("{ fiction: shelves(names: [\"fiction\"]) { name } }")

    val read: fiction.Data => List[String] = _.fiction.map(_.name)
  }

  test("a custom scalar is read as Json when no binding names its type") {
    val read: shelves.Data => List[Option[Json]] = _.shelves.map(_.published)
  }

  test("reading a field the operation does not select does not compile") {
    val errors = compileErrors("""(data: shelves.Data) => data.shelves.map(_.tags)""")

    assert(errors.contains("tags"), errors)
  }

  test("leaving out a variable that must be given a value does not compile") {
    val errors = compileErrors("""shelves(first = 2)""")

    assert(errors.contains("$names must be given a value"), errors)
  }

  test("a variable the operation does not declare does not compile") {
    val errors = compileErrors("""shelves(names = List("a"), limit = 2)""")

    assert(errors.contains("the operation declares no variable '$limit'"), errors)
  }

  test("a single value for a list variable does not compile") {
    val errors = compileErrors("""shelves(names = "fiction")""")

    assert(errors.contains("$names is a list in the schema, so it is written as a collection, not"), errors)
  }

  test("a value of the wrong type for a variable does not compile") {
    val errors = compileErrors("""shelves(names = List("a"), first = "two")""")

    assert(
      errors.contains("$first is a Int in the schema, so it is written as BigInt or Int or Long, not String"),
      errors
    )
  }

  test("a literal argument of the wrong type does not compile") {
    val errors = compileErrors("""petshop.operation("{ pets(first: \"two\") { name } }")""")

    assert(errors.contains("argument 'first' is an Int, but got the string \"two\""), errors)
  }

  test("a variable used where another type is expected does not compile") {
    val errors = compileErrors("""petshop.operation("query Q($first: String) { pets(first: $first) { name } }")""")

    assert(
      errors.contains("variable '$first' is a 'String', which cannot be used where a 'Int' is expected"),
      errors
    )
  }

  test("a fragment that can never apply does not compile") {
    val errors = compileErrors("""petshop.operation("{ pets { ... on Shelter { name } } }")""")

    assert(errors.contains("inline fragment on 'Shelter' can never apply to 'Pet'"), errors)
  }

  test("an operation may interpolate a constant selection that several operations share") {
    inline val petFields = "id name age owner { name }"

    val pets = petshop.operation(
      s"""
        query All {
          pets { $petFields }
        }
      """
    )

    assert(pets.document.contains("pets { id name age owner { name } }"), pets.document)
  }

  test("an operation may be joined from constant strings with +") {
    val pet = petshop.operation("query P($id: ID!) { pet(id: $id) { " + "name" + " } }")

    assertEquals(pet.name, Some("P"))
  }

  test("an operation may be trimmed with stripMargin") {
    val pets = petshop.operation("""{
                                   |  pets { id name age owner { name } }
                                   |}""".stripMargin)

    assert(pets.document.startsWith("{\n  pets"), pets.document)
  }

  test("an operation built from a value the compiler cannot read does not compile") {
    val errors = compileErrors(
      """val fields = List("id").mkString
         petshop.operation(s"{ pets { $fields } }")"""
    )

    assert(errors.contains("the operation must be a constant string"), errors)
  }

  test("a document that is not valid against the schema does not compile") {
    val errors = compileErrors("""library.operation("{ shelves(names: []) { nickname } }")""")

    assert(errors.contains("nickname"), errors)
  }

  test("a binding needs a Decoder for the type it names") {
    val errors = compileErrors(
      """final class Published
         library.read["Date", Published]"""
    )

    assert(errors.contains("Decoder"), errors)
  }

  test("a later binding of the same name replaces an earlier one") {
    val rebound =
      library.read["Date", String].read["Date", java.time.LocalDate].operation("{ shelves(names: []) { published } }")

    val read: rebound.Data => List[Option[java.time.LocalDate]] = _.shelves.map(_.published)
  }
}
