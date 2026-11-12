package net.andimiller.torpor.schema

import io.circe.Json

class IntrospectionSuite extends munit.FunSuite {

  private val schema =
    Schema
      .merge(
        List(
          SdlFragment(
            "shop",
            """type Character { id: ID! name: String! corporation: Corporation! }
              |type Corporation { id: ID! name: String! ticker: String! }
              |enum Species { DOG CAT }
              |extend type Query {
              |  character(id: ID): Character
              |  characters(ids: [ID!]!): [Character!]!
              |}
              |""".stripMargin
          )
        )
      )
      .fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

  private def introspect(query: String): Json =
    Introspection.handle(schema, query).getOrElse(fail("expected an introspection answer"))

  test("__schema reports the root types") {
    val data = introspect("{ __schema { queryType { name } mutationType { name } } }")
    assertEquals(data.hcursor.downField("__schema").downField("queryType").get[String]("name").toOption, Some("Query"))
    assert(data.hcursor.downField("__schema").downField("mutationType").focus.contains(Json.Null))
  }

  test("__type describes an object and its fields") {
    val data = introspect("""{ __type(name: "Character") { kind name fields { name } } }""")
    val tpe  = data.hcursor.downField("__type")
    assertEquals(tpe.get[String]("kind").toOption, Some("OBJECT"))
    assertEquals(
      tpe.downField("fields").values.map(_.flatMap(_.hcursor.get[String]("name").toOption).toList).getOrElse(Nil),
      List("id", "name", "corporation")
    )
  }

  test("the ofType chain keeps nullability and list wrappers") {
    val data       = introspect(
      """{ __type(name: "Query") { fields { name args { name type { kind ofType { kind ofType { kind ofType { name } } } } } } } }"""
    )
    val characters = data.hcursor
      .downField("__type")
      .downField("fields")
      .values
      .map(_.find(_.hcursor.get[String]("name").toOption.contains("characters")))
      .flatMap(_.flatMap(_.hcursor.downField("args").downArray.get[Json]("type").toOption))
      .getOrElse(fail("no characters field"))

    val chain = characters.hcursor
    assertEquals(chain.get[String]("kind").toOption, Some("NON_NULL"))
    assertEquals(chain.downField("ofType").get[String]("kind").toOption, Some("LIST"))
    assertEquals(chain.downField("ofType").downField("ofType").get[String]("kind").toOption, Some("NON_NULL"))
    assertEquals(
      chain.downField("ofType").downField("ofType").downField("ofType").get[String]("name").toOption,
      Some("ID")
    )
  }

  test("fragments are expanded and aliases honoured") {
    val data  = introspect(
      """{
        |  __schema { kinds: types { ...FullType } }
        |}
        |fragment FullType on __Type { kind name }
        |""".stripMargin
    )
    val kinds = data.hcursor.downField("__schema").downField("kinds").values.map(_.toList).getOrElse(Nil)
    assert(kinds.nonEmpty)
    assert(kinds.forall(json => json.hcursor.get[String]("kind").toOption.isDefined), "every type has a kind")
    assert(kinds.exists(_.hcursor.get[String]("name").toOption.contains("Character")), "Character is listed")
  }

  test("enum values are listed") {
    val data = introspect("""{ __type(name: "Species") { kind enumValues { name } } }""")
    val tpe  = data.hcursor.downField("__type")
    assertEquals(tpe.get[String]("kind").toOption, Some("ENUM"))
    assertEquals(
      tpe.downField("enumValues").values.map(_.flatMap(_.hcursor.get[String]("name").toOption).toList).getOrElse(Nil),
      List("DOG", "CAT")
    )
  }

  test("root __typename is the root type name") {
    assertEquals(introspect("{ __typename }").hcursor.get[String]("__typename").toOption, Some("Query"))
  }

  test("a normal query is not treated as introspection") {
    assertEquals(Introspection.handle(schema, "{ character { name } }"), None)
  }

  // ---- tools need more than objects: unions, interfaces, scalars, defaults and deprecations ----------------------

  private val richSchema =
    Schema
      .merge(
        List(
          SdlFragment(
            "rich",
            """scalar Instant @specifiedBy(url: "https://example.com/instant")
              |input Filter { limit: Int = 5 }
              |type Leaf { value: String! }
              |type And { values: [Expression!]! }
              |union Expression = Leaf | And
              |interface Node { id: ID! }
              |type Item implements Node { id: ID! old: String @deprecated(reason: "use new") }
              |enum Colour { RED @deprecated GREEN }
              |extend type Query { search(filter: Filter): Expression }
              |""".stripMargin
          )
        )
      )
      .fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

  private def introspectRich(query: String): Json =
    Introspection.handle(richSchema, query).getOrElse(fail("expected an introspection answer"))

  test("a union reports its members and a scalar its description and spec URL") {
    val union = introspectRich("""{ __type(name: "Expression") { kind possibleTypes { name } } }""").hcursor
      .downField("__type")
    assertEquals(union.get[String]("kind").toOption, Some("UNION"))
    assertEquals(
      union.downField("possibleTypes").values.map(_.flatMap(_.hcursor.get[String]("name").toOption).toList),
      Some(List("Leaf", "And"))
    )

    val scalar = introspectRich("""{ __type(name: "Instant") { kind specifiedByURL } }""").hcursor.downField("__type")
    assertEquals(scalar.get[String]("kind").toOption, Some("SCALAR"))
    assertEquals(scalar.get[String]("specifiedByURL").toOption, Some("https://example.com/instant"))
  }

  test("interfaces and deprecations survive the merge") {
    val item = introspectRich(
      """{ __type(name: "Item") { interfaces { name } fields { name isDeprecated deprecationReason } } }"""
    ).hcursor.downField("__type")
    assertEquals(
      item.downField("interfaces").values.map(_.flatMap(_.hcursor.get[String]("name").toOption).toList),
      Some(List("Node"))
    )
    val old  = item.downField("fields").values.flatMap(_.find(_.hcursor.get[String]("name").toOption.contains("old")))
    assertEquals(old.flatMap(_.hcursor.get[Boolean]("isDeprecated").toOption), Some(true))
    assertEquals(old.flatMap(_.hcursor.get[String]("deprecationReason").toOption), Some("use new"))

    val colour = introspectRich("""{ __type(name: "Colour") { enumValues { name isDeprecated } } }""").hcursor
      .downField("__type")
    val red    =
      colour.downField("enumValues").values.flatMap(_.find(_.hcursor.get[String]("name").toOption.contains("RED")))
    assertEquals(red.flatMap(_.hcursor.get[Boolean]("isDeprecated").toOption), Some(true))
  }

  test("input defaults are GraphQL literals and directives are listed") {
    val filter = introspectRich("""{ __type(name: "Filter") { inputFields { name defaultValue } } }""").hcursor
      .downField("__type")
    val limit  =
      filter.downField("inputFields").values.flatMap(_.find(_.hcursor.get[String]("name").toOption.contains("limit")))
    assertEquals(limit.flatMap(_.hcursor.get[String]("defaultValue").toOption), Some("5"))

    val directives = introspectRich("{ __schema { directives { name } } }").hcursor
      .downField("__schema")
      .downField("directives")
      .values
      .map(_.flatMap(_.hcursor.get[String]("name").toOption).toList)
    assertEquals(directives, Some(List("skip", "include", "deprecated", "specifiedBy")))
  }

  test("__typename works inside an introspection selection") {
    val data = introspectRich("""{ __type(name: "Item") { __typename name } }""")
    assertEquals(data.hcursor.downField("__type").get[String]("__typename").toOption, Some("__Type"))
  }
}
