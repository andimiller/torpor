package net.andimiller.torpor.source

import cats.effect.{Concurrent, Ref}
import cats.syntax.all._
import io.circe.Json
import net.andimiller.hedgehogs.DataGraph
import net.andimiller.hedgehogs.dag.visitor.*
import net.andimiller.hedgehogs.dag.visitor.DagVisitor.RunMode
import net.andimiller.torpor.core.*
import net.andimiller.torpor.schema.{Executable, Schema, SchemaFingerprint}
import net.andimiller.torpor.snapshot.*

/** The result of a durable run: done, or parked in the store with the handles a webhook must complete. */
enum RunOutcome {
  case Finished(response: Response)
  case Suspended(runId: RunId, waiting: Map[String, Json])
}

enum ResumeError(val render: String) {
  case Store(error: StoreError) extends ResumeError(error.render)
  case SchemaDrift(runId: RunId, expected: String, actual: String)
      extends ResumeError(
        s"run ${runId.value} was planned against schema $expected but the schema is now $actual; the client must re-issue"
      )
}

/** Durable runs: hedgehogs owns the run that outlives the request, Torpor owns everything inside a node.
  *
  * Resume claims the run with a compare-and-swap *before* executing, so two webhooks racing for the same run cannot
  * both make downstream calls: the loser gets `Conflict`.
  */
final class Durable[F[_]: Concurrent](
    torpor: Torpor[F],
    store: SnapshotStore[F],
    newRunId: F[RunId]
) {

  def start(
      schema: Schema,
      query: String,
      operationName: Option[String] = None,
      variables: Map[String, Value] = Map.empty
  ): F[Either[QueryError, RunOutcome]] =
    Executable.validate(schema, query, operationName) match {
      case Left(errors)     => Concurrent[F].pure(Left(QueryError.Validation(errors)))
      case Right(validated) =>
        Planner.build(schema, validated, variables) match {
          case Left(problems) => Concurrent[F].pure(Left(QueryError.Planning(problems)))
          case Right(plan)    =>
            val schemaHash = SchemaFingerprint.of(schema)
            SuspendableDagVisitor.start(visitor(plan), RunMode.Flow)(graphOf(plan)).flatMap {
              case RunResult.Finished(done) =>
                Concurrent[F].pure(Right(RunOutcome.Finished(PlanAssembly.response(plan, done.nodeMap))))

              case RunResult.Suspended(snapshot) =>
                newRunId.flatMap { runId =>
                  val stored = StoredRun(schemaHash, plan, snapshot, Revision(1))
                  store.create(runId, stored).flatMap {
                    case Left(error) => Concurrent[F].raiseError(new IllegalStateException(error.render))
                    case Right(_)    => Concurrent[F].pure(Right(RunOutcome.Suspended(runId, waiting(snapshot))))
                  }
                }
            }
        }
    }

  def resume(
      schema: Schema,
      runId: RunId,
      payloads: Map[String, Json]
  ): F[Either[ResumeError, RunOutcome]] =
    store.load(runId).flatMap {
      case None =>
        Concurrent[F].pure(Left(ResumeError.Store(StoreError.NotFound(runId))))

      case Some(stored) =>
        val currentHash = SchemaFingerprint.of(schema)
        if (stored.schemaHash != currentHash) {
          Concurrent[F].pure(
            Left(ResumeError.SchemaDrift(runId, stored.schemaHash, currentHash))
          )
        } else {
          // Claim before running: this is what makes exactly one writer per run true.
          store.replace(runId, stored.revision, stored).flatMap {
            case Left(error)    => Concurrent[F].pure(Left(ResumeError.Store(error)))
            case Right(claimed) =>
              val completions = payloads.map { case (nodeId, payload) => NodeId(nodeId) -> payload }
              SuspendableDagVisitor.resume(visitor(stored.plan))(stored.run, completions).flatMap {
                case RunResult.Finished(done) =>
                  store
                    .delete(runId, claimed)
                    .map(_ => Right(RunOutcome.Finished(PlanAssembly.response(stored.plan, done.nodeMap))))

                case RunResult.Suspended(snapshot) =>
                  store.replace(runId, claimed, stored.copy(run = snapshot, claimed = false)).map {
                    case Left(error) => Left(ResumeError.Store(error))
                    case Right(_)    => Right(RunOutcome.Suspended(runId, waiting(snapshot)))
                  }
              }
          }
        }
    }

  private def waiting(snapshot: DagSnapshot[NodeId, PlanNode, NodeResult, Json, Unit]): Map[String, Json] =
    snapshot.graph.nodeMap.collect { case (id, NodeState.Waiting(_, handle)) =>
      id.value -> handle
    }

  private def graphOf(plan: Plan): DataGraph[NodeId, PlanNode, Unit] =
    plan.nodes.foldLeft(DataGraph.empty[NodeId, PlanNode, Unit]) { (acc, node) =>
      node.dependencies.foldLeft(acc.addNode(node.id, node)) { (withNode, dependency) =>
        withNode.addEdge(dependency, node.id, ())
      }
    }

  private def visitor(plan: Plan): SuspendableDagVisitor[F, NodeId, PlanNode, NodeResult, Unit, Json, Json] = {
    val rootKeys = plan.roots.selections.collect { case NodeField(responseKey, _, id) => id -> responseKey }.toMap

    new SimpleSuspendableDagVisitor[F, NodeId, PlanNode, NodeResult, Unit, Json, Json] {
      def run(id: NodeId, node: PlanNode, inputs: Map[NodeId, NodeResult]): F[StepResult[NodeResult, Json]] =
        torpor.resolverFor(node.source, node.field) match {
          case None           => Concurrent[F].raiseError(new IllegalStateException(s"no resolver for ${node.field.render}"))
          case Some(resolver) =>
            NodeRuntime
              .run(plan, node, node.dependencies.headOption.flatMap(inputs.get), resolver, rootKeys.get(id))
              .map {
                case NodeOutcome.Completed(result) => StepResult.Complete(result)
                case NodeOutcome.Suspended(handle) => StepResult.Suspend(handle)
              }
        }

      def resume(
          id: NodeId,
          node: PlanNode,
          handle: Json,
          payload: Json,
          inputs: Map[NodeId, NodeResult]
      ): F[StepResult[NodeResult, Json]] =
        torpor.resolverFor(node.source, node.field) match {
          case Some(Resolver.Suspending(_, resumeFn)) =>
            attemptResume(resumeFn(handle, payload)).map {
              case Right(Step.Complete(json))  =>
                StepResult.Complete(NodeResult(List(pathOf(rootKeys.get(id)) -> json), cats.data.Chain.empty))
              case Right(Step.Suspend(handle)) => StepResult.Suspend(handle)
              case Left(t)                     =>
                StepResult.Complete(
                  NodeResult(
                    List(pathOf(rootKeys.get(id)) -> Json.Null),
                    cats.data.Chain.one(
                      FieldError(
                        Option(t.getMessage).getOrElse(t.toString),
                        pathOf(rootKeys.get(id)),
                        FieldError.UpstreamUnavailable
                      )
                    )
                  )
                )
            }
          case _                                      =>
            Concurrent[F].raiseError(new IllegalStateException(s"${node.field.render} is not a suspending resolver"))
        }
    }
  }

  /** Resume can throw synchronously too (a handle that will not decode), so both channels are turned into errors. */
  private def attemptResume(
      thunk: => F[Step[Json, Json]]
  ): F[Either[Throwable, Step[Json, Json]]]                      =
    try thunk.attempt
    catch { case t: Throwable => Concurrent[F].pure(Left(t)) }

  private def pathOf(rootKey: Option[String]): List[PathSegment] =
    rootKey.map(key => List(PathSegment.Field(key))).getOrElse(Nil)
}

object Durable {
  def apply[F[_]: Concurrent](torpor: Torpor[F], store: SnapshotStore[F]): F[Durable[F]] =
    Ref
      .of[F, Long](0L)
      .map(counter => new Durable[F](torpor, store, counter.modify(count => (count + 1, RunId(s"run-${count + 1}")))))
}
