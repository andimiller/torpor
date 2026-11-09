package net.andimiller.torpor.source

import net.andimiller.torpor.core.{Argument, FieldCoordinate, InlineField, NodeField, NodeId, Plan, SelectionSet, Value}
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
          NodeField("pets", "pets", NodeId("n0")),
          NodeField("adoptionReport", "adoptionReport", NodeId("n2"))
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
      SelectionSet(List(InlineField("name", "name", SelectionSet.empty), NodeField("owner", "owner", NodeId("n1"))))
    )

    val owner = plan.node(NodeId("n1")).get
    assertEquals(owner.source, "owners")
    assertEquals(owner.field, FieldCoordinate("Pet", "owner"))
    assertEquals(owner.level, 2)
    assertEquals(owner.dependencies, List(NodeId("n0")))
    assertEquals(
      owner.selectionSet,
      SelectionSet(
        List(InlineField("name", "name", SelectionSet.empty), InlineField("email", "email", SelectionSet.empty))
      )
    )

    val report = plan.node(NodeId("n2")).get
    assertEquals(report.source, "reports")
    assertEquals(report.field, FieldCoordinate("Query", "adoptionReport"))
    assertEquals(report.level, 1)
    assertEquals(report.dependencies, Nil)
  }

  test("arguments become literals") {
    val plan = planOf("""{ pets(species: CAT, first: 3) { name } }""")
    val node = plan.nodes.head
    assertEquals(node.args, List(Argument("species", Value.EnumValue("CAT")), Argument("first", Value.IntValue(3))))
  }

  test("aliases move to the response key, not the field") {
    val plan = planOf("""{ dogs: pets(first: 3) { name } }""")
    assertEquals(plan.roots, SelectionSet(List(NodeField("dogs", "pets", NodeId("n0")))))
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

  test("a missing variable is an error") {
    val problems = planErrors("""query Q($first: Int) { pets(first: $first) { name } }""")
    assertEquals(problems.map(_.kind), List(PlanErrorKind.MissingVariable))
    assert(problems.head.render.contains("variable '$first'"))
  }

  test("duplicate selections with the same response key merge into one node") {
    val plan = planOf("""{ pets(first: 3) { name } pets(first: 3) { species } }""")
    assertEquals(plan.nodes.size, 1)
    assertEquals(
      plan.nodes.head.selectionSet,
      SelectionSet(
        List(InlineField("name", "name", SelectionSet.empty), InlineField("species", "species", SelectionSet.empty))
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
          InlineField("name", "name", SelectionSet.empty),
          InlineField("species", "species", SelectionSet.empty),
          NodeField("owner", "owner", NodeId("n1"))
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
      SelectionSet(List(InlineField("species", "species", SelectionSet.empty)))
    )
  }

  test("a narrowing type condition is unsupported, not silently flattened") {
    val problems = planErrors("""{ anything { ... on Pet { name } } }""")
    assertEquals(problems.map(_.kind), List(PlanErrorKind.Unsupported))
    assert(problems.head.render.contains("type conditions are not implemented until M1"))
  }

  test("a type condition on the parent type itself is fine") {
    val plan = planOf("""{ adoptionReport(species: DOG) { ... on AdoptionReport { adoptions } } }""")
    assertEquals(
      plan.nodes.head.selectionSet,
      SelectionSet(List(InlineField("adoptions", "adoptions", SelectionSet.empty)))
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

  test("__typename is rejected until the plan carries static types") {
    val problems = planErrors("{ pets { __typename } }")
    assertEquals(problems.map(_.kind), List(PlanErrorKind.Unsupported))
    assert(problems.head.render.contains("__typename is not implemented until M1"))
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
