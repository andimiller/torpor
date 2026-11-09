package net.andimiller.torpor.macros

import net.andimiller.torpor.core.FieldCoordinate
import net.andimiller.torpor.schema.{Schema, SchemaFingerprint, SdlFragment}

class SchemaMacroSuite extends munit.FunSuite {

  private val shop = schema"""type Character { id: ID! corporation: Corporation! }
                             |type Corporation { id: ID! }
                             |extend type Query { character: Character }
                             |"""

  test("text is the SDL, margins stripped") {
    assert(shop.text.startsWith("type Character"), shop.text)
    assert(!shop.text.contains("|"), shop.text)
  }

  test("coordinates cover definitions and extensions") {
    assertEquals(shop.coordinate("Character", "corporation"), FieldCoordinate("Character", "corporation"))
    assertEquals(shop.coordinate("Query", "character"), FieldCoordinate("Query", "character"))
    assertEquals(shop.coordinates.size, 4)
  }

  test("a missing coordinate fails loudly and lists what exists") {
    val error = intercept[NoSuchElementException](shop.coordinate("Character", "nope"))
    assert(error.getMessage.contains("Character.corporation"), error.getMessage)
  }

  test("the text merges, which is what a golden test needs") {
    val merged = Schema
      .merge(List(SdlFragment("shop", shop.text)))
      .fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)
    assertEquals(SchemaFingerprint.of(merged).length, 64)
    assert(merged.types.contains("Corporation"))
  }

  test("malformed SDL is a compile error") {
    val errors = compileErrors("""schema"type {" """)
    assert(errors.contains("cannot parse SDL"), errors)
  }
}
