package net.andimiller.torpor.core

import cats.data.Chain
import io.circe.Json

/** Response assembly: response keys (aliases), node results keyed by path, no effects.
  *
  * Shared by the request executor and the durable runner, so a resumed run assembles exactly like a fresh one.
  */
object PlanAssembly {

  def response(plan: Plan, results: Map[NodeId, NodeResult]): Response =
    Response(data(plan, results), Chain.fromSeq(results.values.toList.flatMap(_.errors.toList)))

  def data(plan: Plan, results: Map[NodeId, NodeResult]): Json =
    Json.obj(
      plan.roots.selections.map {
        case NodeField(responseKey, _, id)  =>
          val node = plan.node(id).getOrElse(missing(id))
          val path = List(PathSegment.Field(responseKey))
          val raw  = results.get(id).flatMap(_.values.toMap.get(path)).getOrElse(Json.Null)
          responseKey -> assembleValue(node.selectionSet, raw, path, plan, results)
        case InlineField(responseKey, _, _) =>
          // Root fields are always nodes (the planner forces it); this is defensive.
          responseKey -> Json.Null
      }: _*
    )

  private def assembleValue(
      selectionSet: SelectionSet,
      value: Json,
      path: List[PathSegment],
      plan: Plan,
      results: Map[NodeId, NodeResult]
  ): Json =
    if (value.isNull) Json.Null
    else
      value.asArray match {
        case Some(elements)               =>
          Json.fromValues(
            elements.zipWithIndex.map { case (element, index) =>
              assembleValue(selectionSet, element, path :+ PathSegment.Index(index), plan, results)
            }
          )
        case None if selectionSet.isEmpty => value
        case None                         =>
          Json.obj(
            selectionSet.selections.map {
              case InlineField(responseKey, field, selections) =>
                responseKey -> assembleValue(
                  selections,
                  member(value, field),
                  path :+ PathSegment.Field(responseKey),
                  plan,
                  results
                )
              case NodeField(responseKey, _, id)               =>
                val node      = plan.node(id).getOrElse(missing(id))
                val childPath = path :+ PathSegment.Field(responseKey)
                val raw       = results.get(id).flatMap(_.values.toMap.get(childPath)).getOrElse(Json.Null)
                responseKey -> assembleValue(node.selectionSet, raw, childPath, plan, results)
            }: _*
          )
      }

  private def member(value: Json, field: String): Json =
    value.asObject.flatMap(_(field)).getOrElse(Json.Null)

  private def missing(id: NodeId): Nothing =
    throw new IllegalStateException(s"plan references missing node ${id.value}")
}
