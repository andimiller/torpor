package net.andimiller.torpor.macros

import net.andimiller.torpor.core.{Arguments, FieldCoordinate, Sdl}
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

  test("types and fields are members, for definitions and extensions") {
    assertEquals(shop.Character.corporation, FieldCoordinate("Character", "corporation"))
    assertEquals(shop.Corporation.id, FieldCoordinate("Corporation", "id"))
    assertEquals(shop.Query.character, FieldCoordinate("Query", "character"))
  }

  test("a member's type names its parent type and field") {
    val typed: FieldCoordinate.Of["Character", "corporation", Arguments[EmptyTuple], "Corporation"] =
      shop.Character.corporation
    assertEquals(typed, FieldCoordinate("Character", "corporation"))
  }

  test("a type or field the SDL lacks is a compile error") {
    assert(compileErrors("shop.Character.nope").contains("nope"), compileErrors("shop.Character.nope"))
    assert(compileErrors("shop.Charactr.id").contains("Charactr"), compileErrors("shop.Charactr.id"))
  }

  test("the type lists the fields a source must resolve: extension fields and root fields") {
    val pets = schema"""type Pet { id: ID! name: String! }
                       |type Query { pets: [Pet!]! }
                       |extend type Owner { pets: [Pet!]! }
                       |"""
    // `Pet.id` and `Pet.name` are read off whatever produced the pet, so they need no resolver
    summon[pets.type <:< Sdl[("Owner.pets", "Query.pets")]]
    summon[shop.type <:< Sdl["Query.character" *: EmptyTuple]]
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
