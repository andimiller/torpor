package net.andimiller.torpor.client

import io.circe.Json

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

}
