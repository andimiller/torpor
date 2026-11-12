package net.andimiller.torpor.source

import net.andimiller.torpor.core.{
  Argument,
  FieldCoordinate,
  InlineField,
  NodeField,
  NodeId,
  Plan,
  SelectionSet,
  TypeNameField,
  TypeRef,
  Value
}
import net.andimiller.torpor.schema.{SdlFragment, Schema, Executable}

class PlannerSuite extends munit.FunSuite {

  private val fragments = List(
    SdlFragment(
      "pets",
      """
        |enum Species { DOG CAT RABBIT }
        |type Pet { id: ID! name: String! species: Species! }
        |extend type Query {
        |  pets(species: Species, first: Int = 20): [Pet!]!
        |  version: String
        |}
        |""".stripMargin
    ),
    SdlFragment(
      "owners",
      """
        |type Owner { id: ID! name: String! email: String! }
        |extend type Pet { owner: Owner! }
        |extend type Query { anything: Anything }
        |union Anything = Pet | Owner
        |""".stripMargin
    ),
    SdlFragment(
      "reports",
      """
        |type AdoptionReport { species: Species! adoptions: Int! medianDays: Float! }
        |extend type Query { adoptionReport(species: Species!): AdoptionReport! }
        |""".stripMargin
    )
  )

  private val schema =
    Schema.merge(fragments) match {
      case Right(schema) => schema
      case Left(errs)    => fail(errs.toChain.toList.map(_.render).mkString("; "))
    }

  private val nonNullString  = TypeRef.NonNull(TypeRef.Named("String"))
  private val nonNullInt     = TypeRef.NonNull(TypeRef.Named("Int"))
  private val nonNullSpecies = TypeRef.NonNull(TypeRef.Named("Species"))

  private def planOf(query: String, variables: Map[String, Value] = Map.empty): Plan =
    Executable.validate(schema, query) match {
      case Left(errors)     => fail(errors.toChain.toList.map(_.render).mkString("\n"))
      case Right(validated) =>
        Planner.build(schema, validated, variables) match {
          case Right(plan)    => plan
          case Left(problems) => fail(problems.toChain.toList.map(_.render).mkString("\n"))
        }
    }

  private def planErrors(query: String, variables: Map[String, Value] = Map.empty): List[PlanError] =
    Executable.validate(schema, query) match {
      case Left(errors)     => fail(errors.toChain.toList.map(_.render).mkString("\n"))
      case Right(validated) =>
        Planner.build(schema, validated, variables) match {
          case Right(plan)    => fail(s"expected plan errors, got ${plan.nodes.map(_.field.render)}")
          case Left(problems) => problems.toChain.toList
        }
    }

  test("the pet shop query plans to 3 nodes over 2 levels") {
    val plan = planOf("""
      |{
      |  pets(species: DOG, first: 3) { name owner { name email } }
      |  adoptionReport(species: DOG) { adoptions medianDays }
      |}
      |""".stripMargin)

    assertEquals(plan.rootType, "Query")
    assertEquals(plan.nodes.size, 3)
    assertEquals(plan.levels, 2)

    assertEquals(
      plan.roots,
      SelectionSet(
        List(
          NodeField(
            "pets",
            "pets",
            NodeId("n0"),
            TypeRef.NonNull(TypeRef.ListOf(TypeRef.NonNull(TypeRef.Named("Pet"))))
          ),
          NodeField("adoptionReport", "adoptionReport", NodeId("n2"), TypeRef.NonNull(TypeRef.Named("AdoptionReport")))
        )
      )
    )

    val pets = plan.node(NodeId("n0")).get
    assertEquals(pets.source, "pets")
    assertEquals(pets.field, FieldCoordinate("Query", "pets"))
    assertEquals(pets.level, 1)
    assertEquals(pets.dependencies, Nil)
    assertEquals(
      pets.selectionSet,
      SelectionSet(
        List(
          InlineField("name", "name", SelectionSet.empty, nonNullString),
          NodeField("owner", "owner", NodeId("n1"), TypeRef.NonNull(TypeRef.Named("Owner")))
        )
      )
    )

    val owner = plan.node(NodeId("n1")).get
    assertEquals(owner.source, "owners")
    assertEquals(owner.field, FieldCoordinate("Pet", "owner"))
    assertEquals(owner.level, 2)
    assertEquals(owner.dependencies, List(NodeId("n0")))
    assertEquals(
      owner.selectionSet,
      SelectionSet(
        List(
          InlineField("name", "name", SelectionSet.empty, nonNullString),
          InlineField("email", "email", SelectionSet.empty, nonNullString)
        )
      )
    )

    val report = plan.node(NodeId("n2")).get
    assertEquals(report.source, "reports")
    assertEquals(report.field, FieldCoordinate("Query", "adoptionReport"))
    assertEquals(report.level, 1)
    assertEquals(report.dependencies, Nil)
  }

  test("arguments become literals, sorted, with SDL defaults filled") {
    val plan = planOf("""{ pets(species: CAT, first: 3) { name } }""")
    assertEquals(
      plan.nodes.head.args,
      List(Argument("first", Value.IntValue(3)), Argument("species", Value.EnumValue("CAT")))
    )
  }

  test("an omitted argument takes its SDL default") {
    val plan = planOf("""{ pets(species: CAT) { name } }""")
    assertEquals(
      plan.nodes.head.args,
      List(Argument("first", Value.IntValue(20)), Argument("species", Value.EnumValue("CAT")))
    )
  }

  test("aliases move to the response key, not the field") {
    val plan = planOf("""{ dogs: pets(first: 3) { name } }""")
    assertEquals(
      plan.roots,
      SelectionSet(
        List(
          NodeField(
            "dogs",
            "pets",
            NodeId("n0"),
            TypeRef.NonNull(TypeRef.ListOf(TypeRef.NonNull(TypeRef.Named("Pet"))))
          )
        )
      )
    )
    assertEquals(plan.nodes.head.field, FieldCoordinate("Query", "pets"))
  }

  test("variables are substituted once, at planning time") {
    val plan = planOf("""query Q($first: Int) { pets(first: $first) { name } }""", Map("first" -> Value.IntValue(3)))
    assertEquals(plan.nodes.head.args, List(Argument("first", Value.IntValue(3))))
  }

  test("a variable default is used when the client sends nothing") {
    val plan = planOf("""query Q($first: Int = 5) { pets(first: $first) { name } }""")
    assertEquals(plan.nodes.head.args, List(Argument("first", Value.IntValue(5))))
  }

  test("an argument default is used when the variable has no value and no default") {
    val plan = planOf("""query Q($first: Int) { pets(first: $first) { name } }""")
    assertEquals(plan.nodes.head.args, List(Argument("first", Value.IntValue(20))))
  }

  test("a missing variable with no default anywhere is an error") {
    val problems = planErrors("""query Q($species: Species!) { adoptionReport(species: $species) { adoptions } }""")
    assertEquals(problems.map(_.kind), List(PlanErrorKind.MissingVariable))
    assert(problems.head.render.contains("variable '$species'"))
  }

  test("duplicate selections with the same response key merge into one node") {
    val plan = planOf("""{ pets(first: 3) { name } pets(first: 3) { species } }""")
    assertEquals(plan.nodes.size, 1)
    assertEquals(
      plan.nodes.head.selectionSet,
      SelectionSet(
        List(
          InlineField("name", "name", SelectionSet.empty, nonNullString),
          InlineField("species", "species", SelectionSet.empty, nonNullSpecies)
        )
      )
    )
  }

  test("fragments are expanded before the selection set is used as a key") {
    val plan = planOf("""
      |{
      |  pets(first: 3) { ...petFields owner { name } }
      |}
      |fragment petFields on Pet { name species }
      |""".stripMargin)

    assertEquals(
      plan.nodes.head.selectionSet,
      SelectionSet(
        List(
          InlineField("name", "name", SelectionSet.empty, nonNullString),
          InlineField("species", "species", SelectionSet.empty, nonNullSpecies),
          NodeField("owner", "owner", NodeId("n1"), TypeRef.NonNull(TypeRef.Named("Owner")))
        )
      )
    )
  }

  test("@skip and @include are evaluated with variables") {
    val plan = planOf(
      """query Q($show: Boolean!) { pets(first: 3) { name @skip(if: true) species @include(if: $show) } }""",
      Map("show" -> Value.BooleanValue(true))
    )
    assertEquals(
      plan.nodes.head.selectionSet,
      SelectionSet(List(InlineField("species", "species", SelectionSet.empty, nonNullSpecies)))
    )
  }

  test("a union is planned with the fragments restricted to their members") {
    val plan = planOf("""{ anything { __typename ... on Pet { name } ... on Owner { email } } }""")
    assertEquals(
      plan.roots,
      SelectionSet(List(NodeField("anything", "anything", NodeId("n0"), TypeRef.Named("Anything"))))
    )
    assertEquals(
      plan.nodes.head.selectionSet,
      SelectionSet(
        List(
          TypeNameField("__typename", "Anything"),
          InlineField("name", "name", SelectionSet.empty, nonNullString, List("Pet")),
          InlineField("email", "email", SelectionSet.empty, nonNullString, List("Owner"))
        )
      )
    )
  }

  test("a type condition on an object's own type adds no restriction") {
    val plan = planOf("""{ pets(first: 3) { name ... on Pet { species } } }""")
    assertEquals(
      plan.nodes.head.selectionSet,
      SelectionSet(
        List(
          InlineField("name", "name", SelectionSet.empty, nonNullString),
          InlineField("species", "species", SelectionSet.empty, nonNullSpecies)
        )
      )
    )
  }

  test("a type condition on the parent type itself is fine") {
    val plan = planOf("""{ adoptionReport(species: DOG) { ... on AdoptionReport { adoptions } } }""")
    assertEquals(
      plan.nodes.head.selectionSet,
      SelectionSet(List(InlineField("adoptions", "adoptions", SelectionSet.empty, nonNullInt)))
    )
  }

  test("fragment expansion can create a merge conflict the validator cannot see") {
    val problems = planErrors("""
      |{
      |  pets(first: 3) { name }
      |  ...more
      |}
      |fragment more on Query { pets(first: 4) { name } }
      |""".stripMargin)

    assertEquals(problems.map(_.kind).distinct, List(PlanErrorKind.MergeConflict))
    assert(problems.head.render.contains("different arguments"))
  }

  test("a leaf root field still needs a node") {
    val plan = planOf("{ version }")
    assertEquals(plan.nodes.size, 1)
    assertEquals(plan.nodes.head.field, FieldCoordinate("Query", "version"))
    assertEquals(plan.nodes.head.selectionSet, SelectionSet.empty)
  }

  test("__typename becomes a field the assembler answers") {
    val plan = planOf("{ pets { __typename } }")
    assertEquals(plan.nodes.head.selectionSet, SelectionSet(List(TypeNameField("__typename", "Pet"))))
    assertEquals(plan.nodes.head.field, FieldCoordinate("Query", "pets"))
    assertEquals(plan.fieldCount, 2)
  }

  test("planning is deterministic") {
    val query = """{ pets(first: 3) { name owner { email } } }"""
    assertEquals(planOf(query), planOf(query))
  }

  // shortcut: two aliases for the same field plan two nodes, because node sharing across response keys is not
  // implemented yet. It costs a duplicated call, never a wrong answer. Revisit with the executor's call budget.
  test("aliases of the same field are not yet shared") {
    val plan = planOf("{ a: pets(first: 3) { name } b: pets(first: 3) { name } }")
    assertEquals(plan.nodes.size, 2)
  }
}
