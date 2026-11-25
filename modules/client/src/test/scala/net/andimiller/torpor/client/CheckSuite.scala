package net.andimiller.torpor.client

import net.andimiller.torpor.schema.{Executable, Schema, SdlFragment}

/** What an operation selects, as the macro reads it, without the compiler. */
final class CheckSuite extends munit.FunSuite {

  private val schema =
    Schema.merge(
      List(
        SdlFragment(
          "library",
          """enum Genre { FICTION SCIENCE }
            |scalar Date
            |interface Item { id: ID! title: String! }
            |type Person { name: String! born: Int }
            |type Book implements Item { id: ID! title: String! pages: Int! genre: Genre! published: Date rating: Float author: Person! }
            |type Film implements Item { id: ID! title: String! minutes: Int! author: Person! }
            |type Shelf { name: String! items: [Item!]! featured: Item tags: [String]! }
            |type Query { shelf(name: String!): Shelf shelves(first: Int = 10): [Shelf!]! }
            |type Mutation { rename(name: String!, to: String!): Shelf }
            |""".stripMargin
        )
      )
    ) match {
      case Right(merged) => merged
      case Left(errors)  => fail(errors.toChain.toList.map(_.render).mkString("; "))
    }

  private def checked(document: String) =
    Executable.validate(schema, document) match {
      case Right(validated) => (new Check(schema, validated.document), validated.operation)
      case Left(errors)     => fail(errors.toChain.toList.map(_.render).mkString("; "))
    }

  /** The response keys selected beneath `path`, each with whether the response may leave it out. */
  private def keys(document: String, path: String*): List[(String, Boolean)] = {
    val (check, operation) = checked(document)
    val at                 = path.foldLeft(check.root(operation).get) { (selections, key) =>
      check.fields(selections).find(_.key == key).get.beneath
    }
    check.fields(at).map(field => field.key -> field.conditional)
  }

  test("a field selected outside any fragment is always in the response") {
    assertEquals(keys("{ shelves { name tags } }", "shelves"), List("name" -> false, "tags" -> false))
  }

  test("a field of an interface, selected outside any fragment, is always there") {
    assertEquals(keys("{ shelves { items { title } } }", "shelves", "items"), List("title" -> false))
  }

  test("a field selected in one implementer's fragment may be missing") {
    assertEquals(keys("{ shelves { items { ... on Book { pages } } } }", "shelves", "items"), List("pages" -> true))
  }

  test("a field selected for every implementer is always there") {
    assertEquals(
      keys("{ shelves { items { ... on Book { id } ... on Film { id } } } }", "shelves", "items"),
      List("id" -> false)
    )
  }

  test("a named fragment is read like an inline one") {
    assertEquals(
      keys("query Q { shelves { items { ...Length } } } fragment Length on Film { minutes }", "shelves", "items"),
      List("minutes" -> true)
    )
  }

  test("a field selected in every member's fragment is always there, though each selects other fields beneath it") {
    assertEquals(
      keys(
        "{ shelves { items { ... on Book { author { name } } ... on Film { author { born } } } } }",
        "shelves",
        "items"
      ),
      List("author" -> false)
    )
  }

  test("a field selected beneath only one member's fragment may be missing") {
    assertEquals(
      keys(
        "{ shelves { items { ... on Book { author { name } } ... on Film { author { born } } } } }",
        "shelves",
        "items",
        "author"
      ),
      List("name" -> true, "born" -> true)
    )
  }

  test("a field selected beneath every member's fragment is always there") {
    assertEquals(
      keys(
        "{ shelves { items { ... on Book { author { name } } ... on Film { author { name } } } } }",
        "shelves",
        "items",
        "author"
      ),
      List("name" -> false)
    )
  }

  test("a field behind @include may be missing") {
    assertEquals(
      keys("query Q($named: Boolean!) { shelves { name @include(if: $named) } }", "shelves"),
      List("name" -> true)
    )
  }

  test("a field behind @skip may be missing") {
    assertEquals(
      keys("query Q($unnamed: Boolean!) { shelves { name @skip(if: $unnamed) } }", "shelves"),
      List("name" -> true)
    )
  }

  test("an alias is the key the field is read by") {
    assertEquals(keys("""{ fiction: shelf(name: "fiction") { name } }"""), List("fiction" -> false))
  }

  test("__typename is a field like any other") {
    assertEquals(keys("{ shelves { __typename name } }", "shelves"), List("__typename" -> false, "name" -> false))
  }

  test("the possible types of an interface are the types that implement it") {
    val (check, _) = checked("{ shelves { name } }")

    assertEquals(check.possibleTypes("Item"), List("Book", "Film"))
  }

  test("the view of an interface as one implementer has that implementer's fields, always there") {
    val (check, operation) =
      checked("{ shelves { items { __typename title ... on Book { pages } ... on Film { minutes } } } }")
    val shelves            = check.fields(check.root(operation).get).head.beneath
    val items              = check.fields(shelves).head.beneath

    assertEquals(
      check.fields(check.view(items, "Book")).map(field => field.key -> field.conditional),
      List("__typename" -> false, "title" -> false, "pages" -> false)
    )
  }

  test("a mutation starts from the mutation type") {
    val (check, operation) =
      checked("mutation M($name: String!, $to: String!) { rename(name: $name, to: $to) { name } }")

    assertEquals(check.root(operation).map(_.parent), Some("Mutation"))
  }
}
