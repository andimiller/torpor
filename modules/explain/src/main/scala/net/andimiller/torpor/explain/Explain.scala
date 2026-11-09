package net.andimiller.torpor.explain

import net.andimiller.torpor.core.*

/** What the plan will do, as text. Pure: no schema, no effects, no database. Same input as the executor, so it cannot
  * drift from what runs.
  */
object Explain {

  /** Mermaid flowchart: one node per batch call, edges are key flow, the label is `source: Type.field`. */
  def mermaid(plan: Plan): String = {
    val nodes = plan.nodes.map { node =>
      val key = node.id.value
      s"""  $key["${label(plan, node)}"]"""
    }
    val edges = plan.nodes.flatMap { node =>
      node.dependencies.map(dependency => s"  ${dependency.value} --> ${nodeKeyOf(node)}")
    }
    (List("flowchart LR") ++ nodes ++ edges).mkString("\n")
  }

  private def nodeKeyOf(node: PlanNode): String = node.id.value

  private def label(plan: Plan, node: PlanNode): String =
    s"${node.source}: ${node.field.render}"

  /** One line per level: how many calls, to which sources. This is what "why did that query make 7 calls" is answered
    * with, and the test suite pins it.
    */
  def report(plan: Plan): String = {
    val header =
      s"depth ${plan.depth}, ${plan.fieldCount} fields, ${plan.nodes.size} calls over ${plan.levels} level(s)"
    val levels = plan.nodes.groupBy(_.level).toList.sortBy(_._1).map { case (level, atLevel) =>
      val calls = atLevel.groupBy(_.source).toList.sortBy(_._1).map { case (source, nodes) =>
        s"$source x${nodes.size} (${nodes.map(_.field.render).sorted.mkString(", ")})"
      }
      s"level $level: ${calls.mkString("; ")}"
    }
    (header :: levels).mkString("\n")
  }
}
