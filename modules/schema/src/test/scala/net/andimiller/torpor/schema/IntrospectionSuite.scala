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
}
