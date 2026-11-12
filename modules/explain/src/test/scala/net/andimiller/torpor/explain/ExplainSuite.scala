package net.andimiller.torpor.explain

import net.andimiller.torpor.core.*

class ExplainSuite extends munit.FunSuite {

  private val pets  = PlanNode(
    NodeId("n0"),
    "pets",
    FieldCoordinate("Query", "pets"),
    Nil,
    SelectionSet(
      List(
        InlineField("name", "name", SelectionSet.empty, TypeRef.NonNull(TypeRef.Named("String"))),
        NodeField("owner", "owner", NodeId("n1"), TypeRef.Named("Owner"))
      )
    ),
    Nil,
    1
  )
  private val owner = PlanNode(
    NodeId("n1"),
    "owners",
    FieldCoordinate("Pet", "owner"),
    Nil,
    SelectionSet(List(InlineField("name", "name", SelectionSet.empty, TypeRef.NonNull(TypeRef.Named("String"))))),
    List(NodeId("n0")),
    2
  )
  private val plan  =
    Plan("Query", SelectionSet(List(NodeField("pets", "pets", NodeId("n0"), TypeRef.Named("Pet")))), List(pets, owner))

  test("mermaid names each call by source and coordinate, and draws key flow") {
    val mermaid = Explain.mermaid(plan)
    assert(mermaid.startsWith("flowchart LR"), mermaid)
    assert(mermaid.contains("""n0["pets: Query.pets"]"""), mermaid)
    assert(mermaid.contains("""n1["owners: Pet.owner"]"""), mermaid)
    assert(mermaid.contains("n0 --> n1"), mermaid)
  }

  test("report says how many calls per level and per source") {
    val report = Explain.report(plan)
    assert(report.contains("4 fields"), report)
    assert(report.contains("2 calls over 2 level(s)"), report)
    assert(report.contains("level 1: pets x1 (Query.pets)"), report)
    assert(report.contains("level 2: owners x1 (Pet.owner)"), report)
  }
}
