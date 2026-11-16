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
          case Right(Step.Complete(resolved)) =>
            Concurrent[F].pure(NodeOutcome.Completed(completed(path, output(resolved))))
          case Right(Step.Suspend(handle))    => Concurrent[F].pure(NodeOutcome.Suspended(handle))
          case Left(t)                        => failure(hooks, node.field, path, t).map(NodeOutcome.Completed(_))
        }

      case Resolver.Root(run) =>
        run(node.args).attempt.flatMap {
          case Left(t)         => failure(hooks, node.field, path, t).map(NodeOutcome.Completed(_))
          case Right(resolved) =>
            // `""` is the root key: a root node produces one value, and the hook map is keyed like a batch.
            authorize(hooks, node, Map(RootKey -> resolved.json)).map { verdicts =>
              verdicts.get(RootKey) match {
                case Some(Left(error))  => NodeOutcome.Completed(denied(path, error))
                case Some(Right(value)) => NodeOutcome.Completed(completed(path, replaced(resolved, value)))
                case None               => NodeOutcome.Completed(completed(path, output(resolved)))
              }
            }
        }

      // `k` names the batch's key type, so the keys that `key` extracts type-check as what `batch` takes
      case resolver: Resolver.Batch[F, k] =>
        import resolver.{batch, key}
        val positions = parent.toList.flatMap(result => inputPositions(plan, node, result))

        val extracted: List[(List[PathSegment], Either[FieldError, Option[Key[k]]])] =
          positions.map { case (path, parent) =>
            if (parent.json.isNull) (path, Right(None))
            else (path, key(parent).left.map(message => FieldError(message, path, FieldError.InvalidParentValue)))
          }

        // one call per distinct encoding; the batch gets the Scala keys
        val keys                                              =
          extracted.collect { case (_, Right(Some(k))) => k }.distinctBy(_.encoded).map(_.value)
        val call: F[Either[Throwable, Map[String, Resolved]]] =
          try
            NonEmptyList.fromList(keys) match {
              case None      => Concurrent[F].pure(Right(Map.empty[String, Resolved]))
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
            authorize(hooks, node, results.map { case (k, resolved) => k -> resolved.json }).map { verdicts =>
              val values: List[(List[PathSegment], Option[Output], Chain[FieldError])] = extracted.map {
                case (path, Left(error))    => (path, None, Chain.one(error))
                case (path, Right(None))    => (path, None, Chain.empty[FieldError])
                case (path, Right(Some(k))) =>
                  (verdicts.get(k.encoded), results.get(k.encoded)) match {
                    case (Some(Left(error)), _)               =>
                      (path, None, Chain.one(FieldError(error.message, path, error.code)))
                    case (Some(Right(value)), Some(resolved)) => (path, Some(replaced(resolved, value)), Chain.empty)
                    case (Some(Right(value)), None)           => (path, Some(Output(value, None)), Chain.empty)
                    case (None, Some(resolved))               => (path, Some(output(resolved)), Chain.empty[FieldError])
                    case (None, None)                         =>
                      (
                        path,
                        None,
                        Chain.one(FieldError(s"no result for key '${k.encoded}'", path, FieldError.MissingKey))
                      )
                  }
              }
              NodeOutcome.Completed(
                NodeResult(
                  values.map { case (path, resolved, _) => path -> resolved.fold(Json.Null)(_.json) },
                  values.foldLeft(Chain.empty[FieldError]) { case (acc, (_, _, errs)) => acc ++ errs },
                  values.collect { case (path, Some(Output(_, Some(value))), _) => path -> value }.toMap
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

  private def denied(path: List[PathSegment], error: HookError): NodeResult =
    NodeResult(List(path -> Json.Null), Chain.one(FieldError(error.message, path, error.code)))

  /** What a node keeps of one value: its JSON, and the Scala value behind it when there still is one. */
  private final case class Output(json: Json, value: Option[Any])

  private def output(resolved: Resolved): Output = Output(resolved.json, Some(resolved.value))

  /** A hook that changed a value's JSON makes the Scala value stale, so children read the new JSON instead. */
  private def replaced(resolved: Resolved, json: Json): Output =
    if (json == resolved.json) output(resolved) else Output(json, None)

  private def completed(path: List[PathSegment], value: Output): NodeResult                             =
    NodeResult(List(path -> value.json), Chain.empty, value.value.map(path -> _).toMap)

  /** Where this node's values sit inside its parent's output. Deterministic, so execution and assembly agree. */
  def inputPositions(plan: Plan, node: PlanNode, parent: NodeResult): List[(List[PathSegment], Parent)] =
    node.dependencies.headOption.flatMap(plan.node) match {
      case None             => Nil
      case Some(parentNode) =>
        parent.values.flatMap { case (path, value) =>
          val produced = parent.produced.get(path).map(Produced(parentNode.field, _))
          childPositions(value, produced, parentNode.selectionSet, path, node.id)
        }
    }

  /** Positions of a specific child node's parent values, in response order. Inline fields only contribute a path
    * prefix; only the matching `NodeField` yields values, otherwise sibling scalars would be mistaken for parent
    * values. A selection that only applies to some concrete types (a fragment on a union member) yields nothing for the
    * other types.
    *
    * The Scala value is walked next to the JSON, through `Option`s and collections, so a parent that its resolver
    * produced directly reaches the child as that value. Inside an inline field there is no Scala value to walk, so a
    * child there gets only the JSON.
    */
  def childPositions(
      value: Json,
      produced: Option[Produced],
      selectionSet: SelectionSet,
      path: List[PathSegment],
      childId: NodeId
  ): List[(List[PathSegment], Parent)] =
    value.asArray match {
      case Some(elements)       =>
        val producedElements = produced.flatMap { p =>
          elementsOf(p.value).filter(_.size == elements.size).map(_.map(Produced(p.by, _)))
        }
        elements.toList.zipWithIndex.flatMap { case (element, index) =>
          childPositions(
            element,
            producedElements.map(_(index)),
            selectionSet,
            path :+ PathSegment.Index(index),
            childId
          )
        }
      case None if value.isNull => Nil
      case None                 =>
        selectionSet.selections.flatMap {
          case InlineField(responseKey, field, selections, _, _)                                         =>
            childPositions(member(value, field), None, selections, path :+ PathSegment.Field(responseKey), childId)
          case NodeField(responseKey, _, id, _, appliesTo) if id == childId && applies(appliesTo, value) =>
            // The key comes from the enclosing parent value, never from the parent's data for that field.
            List(
              (path :+ PathSegment.Field(responseKey)) -> Parent(
                value,
                produced.map(p => p.copy(value = present(p.value)))
              )
            )
          case _                                                                                         => Nil
        }
    }

  private def elementsOf(value: Any): Option[Vector[Any]] = value match {
    case Some(inner)         => elementsOf(inner)
    case values: Iterable[?] => Some(values.toVector)
    case _                   => None
  }

  private def present(value: Any): Any = value match {
    case Some(inner) => present(inner)
    case other       => other
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
