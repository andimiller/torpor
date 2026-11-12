package net.andimiller.torpor.schema

import cats.data.NonEmptyChain

class SchemaMergeSuite extends munit.FunSuite {

  private val pets = SdlFragment(
    "pets",
    """
      |enum Species { DOG CAT RABBIT }
      |type Pet {
      |  id: ID!
      |  name: String!
      |  species: Species!
      |}
      |extend type Query {
      |  pets(species: Species, first: Int = 20): [Pet!]!
      |}
      |""".stripMargin
  )

  private val owners = SdlFragment(
    "owners",
    """
      |type Owner { id: ID! name: String! email: String! }
      |extend type Pet { owner: Owner! }
      |extend type Owner { pets: [Pet!]! }
      |""".stripMargin
  )

  private val reports = SdlFragment(
    "reports",
    """
      |type AdoptionReport { species: Species! adoptions: Int! medianDays: Float! }
      |extend type Query { adoptionReport(species: Species!): AdoptionReport! }
      |""".stripMargin
  )

  private def mergeOrFail(fragments: SdlFragment*): Schema =
    Schema.merge(fragments.toList) match {
      case Right(schema) => schema
      case Left(errs)    => fail(errs.toChain.toList.map(_.render).mkString("; "))
    }

  private def errorsOf(fragments: SdlFragment*): List[String] =
    Schema.merge(fragments.toList) match {
      case Right(schema) => fail(s"expected errors, got ${schema.types.keys.mkString(", ")}")
      case Left(errs)    => errs.toChain.toList.map(_.render)
    }

  test("merges the pet shop fragments into one schema") {
    val schema = mergeOrFail(pets, owners, reports)

    assertEquals(schema.query, Some("Query"))
    assertEquals(schema.mutation, None)
    assertEquals(schema.rootFields.map(_.name).toSet, Set("pets", "adoptionReport"))

    val pet = schema.types("Pet")
    assertEquals(pet.kind, TypeKind.ObjectType)
    assertEquals(pet.fields.map(_.name), List("id", "name", "species", "owner"))
    assertEquals(pet.origin.source, "pets")

    val owner = schema.types("Owner")
    assertEquals(owner.fields.map(_.name), List("id", "name", "email", "pets"))

    val species = schema.types("Species")
    assertEquals(species.kind, TypeKind.EnumType)
    assertEquals(species.enumValues.map(_.name), List("DOG", "CAT", "RABBIT"))

    val report = schema.types("AdoptionReport")
    assertEquals(report.fields.map(_.name), List("species", "adoptions", "medianDays"))
  }

  test("fields from an extension are marked, fields from the definition are not") {
    val schema = mergeOrFail(pets, owners)
    val pet    = schema.types("Pet")

    assertEquals(pet.fields.filter(_.fromExtension).map(_.name), List("owner"))
    assertEquals(pet.fields.filterNot(_.fromExtension).map(_.name), List("id", "name", "species"))
    assertEquals(pet.fields.find(_.name == "owner").map(_.origin.source), Some("owners"))
  }

  test("translates list and non-null wrappers") {
    val schema    = mergeOrFail(pets)
    val petsField = schema.rootFields.find(_.name == "pets").get

    assertEquals(
      petsField.tpe,
      TypeRef.NonNull(TypeRef.ListOf(TypeRef.NonNull(TypeRef.Named("Pet"))))
    )
    assertEquals(
      petsField.args.map(a => a.name -> a.tpe),
      List("species" -> TypeRef.Named("Species"), "first" -> TypeRef.Named("Int"))
    )
  }

  test("synthesizes a root type that is only ever extended") {
    val schema = mergeOrFail(pets, owners, reports)
    assertEquals(schema.types("Query").origin, Origin.synthesized)
    assertEquals(schema.types("Query").fields.size, 2)
  }

  test("a type defined by two sources is an error naming both") {
    val other  = SdlFragment("owners", "type Pet { id: ID! }")
    val errors = errorsOf(pets, other)

    assertEquals(errors.size, 1)
    assert(clue(errors.head).contains("type 'Pet' is defined twice"))
    assert(clue(errors.head).contains("pets:"))
    assert(clue(errors.head).contains("owners:"))
  }

  test("an extension redeclaring a field is an error naming both") {
    val other  = SdlFragment("owners", "extend type Pet { name: String! }")
    val errors = errorsOf(pets, other)

    assertEquals(errors.size, 1)
    assert(errors.head.contains("field 'Pet.name' is declared twice"))
    assert(errors.head.contains("pets:"))
    assert(errors.head.contains("owners:"))
  }

  test("two extensions declaring the same field is an error") {
    val a      = SdlFragment("a", "type Pet { id: ID! }\nextend type Pet { ownerId: ID! }")
    val b      = SdlFragment("b", "extend type Pet { ownerId: ID! }")
    val errors = errorsOf(a, b)

    assertEquals(errors.size, 1)
    assert(errors.head.contains("field 'Pet.ownerId' is declared twice"))
  }

  test("extending an undefined non-root type is an error") {
    val errors = errorsOf(SdlFragment("owners", "extend type Pet { owner: ID }"))
    assertEquals(errors.size, 1)
    assert(errors.head.contains("cannot extend type 'Pet'"))
  }

  test("extending a type as the wrong kind is an error") {
    val errors =
      errorsOf(SdlFragment("pets", "enum Species { DOG }"), SdlFragment("x", "extend type Species { a: Int }"))

    assertEquals(errors.size, 1)
    assert(errors.head.contains("extends 'Species' as a object"))
    assert(errors.head.contains("defines it as a enum"))
  }

  test("an explicit schema block names the root types") {
    val schema = mergeOrFail(SdlFragment("s", "schema { query: Root }\ntype Root { a: Int }"))
    assertEquals(schema.query, Some("Root"))
    assertEquals(schema.rootFields.map(_.name), List("a"))
  }

  test("an explicit root naming a missing type is an error") {
    val errors = errorsOf(SdlFragment("s", "schema { query: Nope }"))
    assertEquals(errors.size, 1)
    assert(errors.head.contains("root query type 'Nope' is not defined"))
  }

  test("two sources disagreeing on the root type is an error") {
    val errors = errorsOf(
      SdlFragment("a", "schema { query: A }\ntype A { x: Int }"),
      SdlFragment("b", "schema { query: B }\ntype B { y: Int }")
    )
    assertEquals(errors.size, 1)
    assert(errors.head.contains("root query is declared as 'A'"))
  }

  test("a client query in an SDL fragment is rejected, not ignored") {
    val errors = errorsOf(SdlFragment("pets", "{ pets { name } }"))
    assertEquals(errors.size, 1)
    assert(errors.head.contains("found a query operation definition"))
  }

  test("custom directive definitions are rejected, not ignored") {
    val errors = errorsOf(SdlFragment("pets", "directive @auth on FIELD_DEFINITION"))
    assertEquals(errors.size, 1)
    assert(errors.head.contains("custom directive '@auth' is not supported"))
  }

  test("a syntax error is reported with its source") {
    val errors = errorsOf(SdlFragment("pets", "type Pet {"))
    assertEquals(errors.size, 1)
    assert(errors.head.startsWith("pets"))
    assert(errors.head.contains("cannot parse SDL"))
  }

  test("independent problems are all reported, not just the first") {
    val bad    = SdlFragment("bad", "type Pet { id: ID! }\ntype Pet { id: ID! }\nextend type Nope { a: Int }")
    val errors = errorsOf(pets, bad)

    assert(clue(errors).exists(_.contains("type 'Pet' is defined twice")))
    assert(clue(errors).exists(_.contains("cannot extend type 'Nope'")))
  }
}
