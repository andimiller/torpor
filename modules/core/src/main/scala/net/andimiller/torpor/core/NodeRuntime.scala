package net.andimiller.torpor.core

import cats.data.{Chain, NonEmptyList}
import cats.effect.Concurrent
import cats.syntax.all._
import io.circe.Json

/** What one visit to a node produced: a result, or a suspension handle. */
enum NodeOutcome {
  case Completed(result: NodeResult)
  case Suspended(handle: Json)
}

/** Node execution: one batch call, keys deduped, errors as values. Shared by the request executor and the durable
  * runner so both behave identically; the durable one is the only one that can see `Suspended`.
  */
object NodeRuntime {

  def run[F[_]: Concurrent](
      plan: Plan,
      node: PlanNode,
      parent: Option[NodeResult],
      resolver: Resolver[F],
      rootKey: Option[String]
  ): F[NodeOutcome] =
    // Boundary: `defer` moves construction of the resolver's F inside the effect, so a synchronous throw (the DSL
    // throws when arguments or keys do not decode) becomes a field error instead of escaping into hedgehogs' fail-fast
    // segment, which would cancel in-flight fibers and lose the snapshot.
    try
      execute(plan, node, parent, resolver, rootKey)
        .handleErrorWith(t => Concurrent[F].pure(NodeOutcome.Completed(upstreamFailure(pathOf(rootKey), t))))
    catch {
      // A synchronous throw happens while the resolver's effect is being built; catching it here keeps hedgehogs' fair
      // catch clause honest (it must never see a raised node).
      case t: Throwable => Concurrent[F].pure(NodeOutcome.Completed(upstreamFailure(pathOf(rootKey), t)))
    }

  private def execute[F[_]: Concurrent](
      plan: Plan,
      node: PlanNode,
      parent: Option[NodeResult],
      resolver: Resolver[F],
      rootKey: Option[String]
  ): F[NodeOutcome] =
    resolver match {
      case Resolver.Suspending(start, _) =>
        start(node.args).attempt.map {
          case Right(Step.Complete(json))  =>
            NodeOutcome.Completed(NodeResult(List(pathOf(rootKey) -> json), Chain.empty))
          case Right(Step.Suspend(handle)) => NodeOutcome.Suspended(handle)
          case Left(t)                     =>
            NodeOutcome.Completed(upstreamFailure(pathOf(rootKey), t))
        }

      case Resolver.Root(run) =>
        run(node.args).attempt.map {
          case Right(json) => NodeOutcome.Completed(NodeResult(List(pathOf(rootKey) -> json), Chain.empty))
          case Left(t)     => NodeOutcome.Completed(upstreamFailure(pathOf(rootKey), t))
        }

      case Resolver.Batch(key, batch) =>
        val positions = parent.toList.flatMap(result => inputPositions(plan, node, result))

        val extracted: List[(List[PathSegment], Either[FieldError, Option[String]])] =
          positions.map { case (path, value) =>
            if (value.isNull) (path, Right(None): Either[FieldError, Option[String]])
            else
              key(value) match {
                case Left(message) => (path, Left(FieldError(message, path, FieldError.InvalidParentValue)))
                case Right(k)      => (path, Right(Some(k)))
              }
          }

        val keys                                       = extracted.collect { case (_, Right(Some(k))) => k }.distinct
        val call: F[Either[String, Map[String, Json]]] = NonEmptyList.fromList(keys) match {
          case None      => Concurrent[F].pure(Right(Map.empty[String, Json]))
          case Some(nel) => batch(nel).attempt.map(_.left.map(t => Option(t.getMessage).getOrElse(t.toString)))
        }

        call.map {
          case Left(message) =>
            NodeOutcome.Completed(
              NodeResult(
                extracted.map { case (path, _) => path -> Json.Null },
                Chain.fromSeq(extracted.map { case (path, _) =>
                  FieldError(message, path, FieldError.UpstreamUnavailable)
                })
              )
            )

          case Right(results) =>
            val values: List[(List[PathSegment], Json, Chain[FieldError])] = extracted.map {
              case (path, Left(error))    => (path, Json.Null, Chain.one(error))
              case (path, Right(None))    => (path, Json.Null, Chain.empty[FieldError])
              case (path, Right(Some(k))) =>
                results.get(k) match {
                  case Some(json) => (path, json, Chain.empty[FieldError])
                  case None       =>
                    (path, Json.Null, Chain.one(FieldError(s"no result for key '$k'", path, FieldError.MissingKey)))
                }
            }
            NodeOutcome.Completed(
              NodeResult(
                values.map { case (path, json, _) => path -> json },
                values.foldLeft(Chain.empty[FieldError]) { case (acc, (_, _, errs)) => acc ++ errs }
              )
            )
        }
    }

  /** Where this node's values sit inside its parent's output. Deterministic, so execution and assembly agree. */
  def inputPositions(plan: Plan, node: PlanNode, parent: NodeResult): List[(List[PathSegment], Json)] =
    node.dependencies.headOption.flatMap(plan.node) match {
      case None             => Nil
      case Some(parentNode) =>
        parent.values.flatMap { case (path, value) =>
          childPositions(value, parentNode.selectionSet, path, node.id)
        }
    }

  /** Positions of a specific child node's parent values, in response order. Inline fields only contribute a path
    * prefix; only the matching `NodeField` yields values, otherwise sibling scalars would be mistaken for parent
    * values.
    */
  def childPositions(
      value: Json,
      selectionSet: SelectionSet,
      path: List[PathSegment],
      childId: NodeId
  ): List[(List[PathSegment], Json)] =
    value.asArray match {
      case Some(elements)       =>
        elements.toList.zipWithIndex.flatMap { case (element, index) =>
          childPositions(element, selectionSet, path :+ PathSegment.Index(index), childId)
        }
      case None if value.isNull => Nil
      case None                 =>
        selectionSet.selections.flatMap {
          case InlineField(responseKey, field, selections)    =>
            childPositions(member(value, field), selections, path :+ PathSegment.Field(responseKey), childId)
          case NodeField(responseKey, _, id) if id == childId =>
            // The key comes from the enclosing parent value, never from the parent's data for this field.
            List((path :+ PathSegment.Field(responseKey)) -> value)
          case NodeField(_, _, _)                             => Nil
        }
    }

  /** A root node's response path, supplied from `Plan.roots` so errors carry an absolute path. */
  private def pathOf(rootKey: Option[String]): List[PathSegment] =
    rootKey.map(key => List(PathSegment.Field(key))).getOrElse(Nil)

  private def upstreamFailure(path: List[PathSegment], t: Throwable): NodeResult =
    NodeResult(
      List(path -> Json.Null),
      Chain.one(FieldError(Option(t.getMessage).getOrElse(t.toString), path, FieldError.UpstreamUnavailable))
    )

  def member(value: Json, field: String): Json =
    value.asObject.flatMap(_(field)).getOrElse(Json.Null)
}
