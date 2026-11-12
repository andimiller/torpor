package net.andimiller.torpor.core

import cats.data.{Chain, NonEmptyList}
import cats.effect.Concurrent
import cats.syntax.all._
import io.circe.Json

import scala.concurrent.duration.*

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
      rootKey: Option[String],
      hooks: Hooks[F]
  ): F[NodeOutcome] = {
    val path = pathOf(rootKey)

    // Boundary: a synchronous throw happens while the resolver's effect is being built (the DSL throws when arguments
    // or keys do not decode); catching it here keeps hedgehogs' fair catch clause honest.
    try {
      val started = System.nanoTime()
      hooks
        .nodeSpan(node.field, node.level)
        .surround {
          execute(plan, node, parent, resolver, rootKey, path, hooks)
            .flatMap(outcome => hooks.onNode(node.field, node.level, (System.nanoTime() - started).nanos).as(outcome))
        }
        .handleErrorWith(t => failure(hooks, node.field, path, t).map(NodeOutcome.Completed(_)))
    } catch {
      case t: Throwable => failure(hooks, node.field, path, t).map(NodeOutcome.Completed(_))
    }
  }

  private def failure[F[_]: Concurrent](
      hooks: Hooks[F],
      coordinate: FieldCoordinate,
      path: List[PathSegment],
      t: Throwable
  ): F[NodeResult] =
    hooks.onError(coordinate, t).map { error =>
      NodeResult(List(path -> Json.Null), Chain.one(FieldError(error.message, path, error.code)))
    }

  private def execute[F[_]: Concurrent](
      plan: Plan,
      node: PlanNode,
      parent: Option[NodeResult],
      resolver: Resolver[F],
      rootKey: Option[String],
      path: List[PathSegment],
      hooks: Hooks[F]
  ): F[NodeOutcome] =
    resolver match {
      case Resolver.Suspending(start, _) =>
        start(node.args).attempt.flatMap {
          case Right(Step.Complete(json))  =>
            Concurrent[F].pure(NodeOutcome.Completed(NodeResult(List(path -> json), Chain.empty)))
          case Right(Step.Suspend(handle)) => Concurrent[F].pure(NodeOutcome.Suspended(handle))
          case Left(t)                     => failure(hooks, node.field, path, t).map(NodeOutcome.Completed(_))
        }

      case Resolver.Root(run) =>
        run(node.args).attempt.flatMap {
          case Left(t)     => failure(hooks, node.field, path, t).map(NodeOutcome.Completed(_))
          case Right(json) =>
            // `""` is the root key: a root node produces one value, and the hook map is keyed like a batch.
            authorize(hooks, node, Map(RootKey -> json)).map { verdicts =>
              verdicts.get(RootKey) match {
                case Some(Left(error))  =>
                  NodeOutcome.Completed(denied(path, error))
                case Some(Right(value)) =>
                  NodeOutcome.Completed(NodeResult(List(path -> value), Chain.empty))
                case None               =>
                  NodeOutcome.Completed(NodeResult(List(path -> json), Chain.empty))
              }
            }
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

        val keys                                          = extracted.collect { case (_, Right(Some(k))) => k }.distinct
        val call: F[Either[Throwable, Map[String, Json]]] =
          try
            NonEmptyList.fromList(keys) match {
              case None      => Concurrent[F].pure(Right(Map.empty[String, Json]))
              case Some(nel) => batch(node.args, nel).attempt
            }
          catch {
            // a batch that throws while building its effect (rather than inside it)
            case t: Throwable => Concurrent[F].pure(Left(t))
          }

        call.flatMap {
          case Left(t) =>
            hooks.onError(node.field, t).map { error =>
              NodeOutcome.Completed(
                NodeResult(
                  extracted.map { case (path, _) => path -> Json.Null },
                  Chain.fromSeq(extracted.map { case (path, _) => FieldError(error.message, path, error.code) })
                )
              )
            }

          case Right(results) =>
            authorize(hooks, node, results).map { verdicts =>
              val values: List[(List[PathSegment], Json, Chain[FieldError])] = extracted.map {
                case (path, Left(error))    => (path, Json.Null, Chain.one(error))
                case (path, Right(None))    => (path, Json.Null, Chain.empty[FieldError])
                case (path, Right(Some(k))) =>
                  verdicts.get(k) match {
                    case Some(Left(error))  => (path, Json.Null, Chain.one(FieldError(error.message, path, error.code)))
                    case Some(Right(value)) => (path, value, Chain.empty[FieldError])
                    case None               =>
                      results.get(k) match {
                        case Some(json) => (path, json, Chain.empty[FieldError])
                        case None       =>
                          (
                            path,
                            Json.Null,
                            Chain.one(FieldError(s"no result for key '$k'", path, FieldError.MissingKey))
                          )
                      }
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
    }

  private val RootKey = ""

  private def authorize[F[_]: Concurrent](
      hooks: Hooks[F],
      node: PlanNode,
      values: Map[String, Json]
  ): F[Map[String, Either[HookError, Json]]] =
    if (values.isEmpty) Concurrent[F].pure(Map.empty) else hooks.authorize(node.field, node.args, values)

  private def denied(path: List[PathSegment], error: HookError): NodeResult                           =
    NodeResult(List(path -> Json.Null), Chain.one(FieldError(error.message, path, error.code)))

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
    * values. A selection that only applies to some concrete types (a fragment on a union member) yields nothing for the
    * other types.
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
          case InlineField(responseKey, field, selections, _, _)                                         =>
            childPositions(member(value, field), selections, path :+ PathSegment.Field(responseKey), childId)
          case NodeField(responseKey, _, id, _, appliesTo) if id == childId && applies(appliesTo, value) =>
            // The key comes from the enclosing parent value, never from the parent's data for that field.
            List((path :+ PathSegment.Field(responseKey)) -> value)
          case _                                                                                         => Nil
        }
    }

  /** A selection with an empty `appliesTo` applies to every type; otherwise its `__typename` must be listed. */
  def applies(appliesTo: List[String], value: Json): Boolean =
    appliesTo.isEmpty || member(value, "__typename").asString.exists(appliesTo.contains)

  /** A root node's response path, supplied from `Plan.roots` so errors carry an absolute path. */
  private def pathOf(rootKey: Option[String]): List[PathSegment] =
    rootKey.map(key => List(PathSegment.Field(key))).getOrElse(Nil)

  def member(value: Json, field: String): Json =
    value.asObject.flatMap(_(field)).getOrElse(Json.Null)
}
