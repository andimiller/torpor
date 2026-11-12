package net.andimiller.torpor.core

import cats.data.Chain
import io.circe.Json

/** Response assembly: response keys (aliases), node results keyed by path, no effects.
  *
  * Shared by the request executor and the durable runner, so a resumed run assembles exactly like a fresh one.
  *
  * Null bubbling is here because only assembly knows the response tree: a null in a non-null position reports an error
  * at that position and nulls the nearest nullable ancestor (the whole of `data` if there is none).
  */
object PlanAssembly {

  def response(plan: Plan, results: Map[NodeId, NodeResult]): Response = {
    val assembler = new Assembler(plan, results)
    val data      = assembler.data
    Response(data, Chain.fromSeq(results.values.toList.flatMap(_.errors.toList)) ++ Chain.fromSeq(assembler.errors))
  }

  private final class Assembler(plan: Plan, results: Map[NodeId, NodeResult]) {
    private val collected = List.newBuilder[FieldError]

    /** Errors raised by join failures, so a null that is already an error is not reported twice. */
    private var known: Set[List[PathSegment]] = results.values.toList.flatMap(_.errors.toList).map(_.path).toSet

    def errors: List[FieldError] = collected.result()

    def data: Json = {
      val roots = plan.roots.selections.map { selection =>
        // the selection adds its own response key to the path, including for root fields
        val (key, json, bubbled) = selectionValue(selection, Json.Null, Nil)
        (key, json, bubbled)
      }
      if (roots.exists(_._3)) Json.Null
      else Json.obj(roots.map { case (key, json, _) => key -> json }: _*)
    }

    /** One selection on an object value: its response key, its assembled value, and whether that value must bubble. */
    private def selectionValue(
        selection: Selection,
        parent: Json,
        path: List[PathSegment]
    ): (String, Json, Boolean) =
      selection match {
        case TypeNameField(responseKey, staticName) =>
          val name = member(parent, "__typename").asString.getOrElse(staticName)
          (responseKey, Json.fromString(name), false)

        case InlineField(responseKey, field, selections, tpe, _) =>
          val childPath       = path :+ PathSegment.Field(responseKey)
          val (json, bubbled) = assemble(selections, member(parent, field), childPath, tpe)
          (responseKey, json, bubbled)

        case NodeField(responseKey, _, id, tpe, _) =>
          val childPath       = path :+ PathSegment.Field(responseKey)
          val raw             = results.get(id).flatMap(_.values.toMap.get(childPath)).getOrElse(Json.Null)
          val (json, bubbled) = plan.node(id) match {
            case None       => (Json.Null, tpe.isNonNull)
            case Some(node) => assemble(node.selectionSet, raw, childPath, tpe)
          }
          (responseKey, json, bubbled)
      }

    /** Assemble one value against its declared type. Returns the value and whether it bubbles to its parent. */
    private def assemble(
        selectionSet: SelectionSet,
        value: Json,
        path: List[PathSegment],
        tpe: TypeRef
    ): (Json, Boolean) = {
      val nonNull = tpe.isNonNull
      val core    = tpe match {
        case TypeRef.NonNull(inner) => inner
        case other                  => other
      }

      if (value.isNull) {
        if (nonNull) {
          noteNull(path)
          (Json.Null, true)
        } else (Json.Null, false)
      } else
        core match {
          case TypeRef.ListOf(element) =>
            value.asArray match {
              case None           => if (nonNull) (Json.Null, true) else (Json.Null, false)
              case Some(elements) =>
                val assembled = elements.toList.zipWithIndex.map { case (item, index) =>
                  assemble(selectionSet, item, path :+ PathSegment.Index(index), element)
                }
                if (assembled.exists(_._2)) {
                  // An element was null in a non-null element position: the list becomes null.
                  if (nonNull) (Json.Null, true) else (Json.Null, false)
                } else (Json.fromValues(assembled.map(_._1)), false)
            }

          case TypeRef.Named(_) =>
            if (selectionSet.isEmpty) (value, false)
            else {
              var bubbled    = false
              // fragments on other members contribute nothing, rather than a null field
              val applicable =
                selectionSet.selections.filter(selection => NodeRuntime.applies(selection.appliesTo, value))
              val fields     = applicable.map { selection =>
                val (key, json, childBubbled) = selectionValue(selection, value, path)
                if (childBubbled) bubbled = true
                key -> json
              }
              if (bubbled) {
                if (nonNull) (Json.Null, true) else (Json.Null, false)
              } else (Json.obj(fields: _*), false)
            }
        }
    }

    private def noteNull(path: List[PathSegment]): Unit =
      if (!known(path)) {
        known += path
        collected += FieldError("null value in non-null field", path, FieldError.NullInNonNull)
      }

    private def member(value: Json, field: String): Json =
      value.asObject.flatMap(_(field)).getOrElse(Json.Null)
  }
}
