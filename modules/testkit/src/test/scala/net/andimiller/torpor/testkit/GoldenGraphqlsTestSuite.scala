package net.andimiller.torpor.testkit

import java.nio.file.{Files, Path}

/** What the golden renderer promises: one deterministic document, a missing file created, a match silent, a drift a
  * readable failure.
  */
final class GoldenGraphqlsTestSuite extends munit.FunSuite {

  private val characters =
    """type Character { id: ID! name: String! }
      |extend type Query { character(id: ID): Character }""".stripMargin

  private val universe =
    """type Corporation { id: ID! ticker: String! }
      |extend type Character { corporation: Corporation! }""".stripMargin

  private val expected =
    """type Character {
      |  id: ID!
      |  name: String!
      |  corporation: Corporation!
      |}
      |
      |type Corporation {
      |  id: ID!
      |  ticker: String!
      |}
      |
      |type Query {
      |  character(id: ID): Character
      |}
      |""".stripMargin

  private def tempFile(name: String): Path = {
    val dir = Files.createTempDirectory("torpor-testkit")
    dir.toFile.deleteOnExit()
    dir.resolve(name)
  }

  test("merges the fragments and renders one document, types sorted and fields in declaration order") {
    assertEquals(GoldenGraphqls.render(List(characters, universe)), expected)
  }

  test("source order does not matter: the golden file is stable") {
    assertEquals(GoldenGraphqls.render(List(universe, characters)), expected)
  }

  test("an absent golden file is written, not failed") {
    val file = tempFile("nested/full.graphqls")
    GoldenGraphqls.assertMatches(file, List(characters, universe))
    assertEquals(Files.readString(file), expected)
  }

  test("a matching golden file passes, a drifted one fails with the diff") {
    val file = tempFile("full.graphqls")
    Files.writeString(file, expected)

    GoldenGraphqls.assertMatches(file, List(characters, universe))

    val error = intercept[munit.ComparisonFailException] {
      GoldenGraphqls.assertMatches(file, List(characters.replace("name: String!", "name: String"), universe))
    }
    assert(error.message.contains("name: String"), error.message)
    assert(error.message.contains("full.graphqls"), error.message)
  }

  test("a fragment that does not merge fails with every schema error") {
    val error = intercept[munit.FailException] {
      GoldenGraphqls.render(List(characters, "type Character { id: ID! }"))
    }
    assert(error.message.contains("Character"), error.message)
  }

  test("renderer prints lists, arguments, interfaces and descriptions") {
    def render(sdl: String) = GoldenGraphqls.render(List(sdl))
    val rendered            = render(
      "\"\"\"A thing.\"\"\"\ninterface Named { name: String! }\n" +
        "type Thing implements Named { name: String! tags(limit: Int!): [String]! }"
    )
    assert(rendered.contains("\"\"\"A thing.\"\"\""), rendered)
    assert(rendered.contains("type Thing implements Named {"), rendered)
    assert(rendered.contains("  tags(limit: Int!): [String]!"), rendered)

  }
}
