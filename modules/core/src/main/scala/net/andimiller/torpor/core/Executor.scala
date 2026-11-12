package net.andimiller.torpor.core

import cats.effect.Concurrent
import cats.syntax.all._
import net.andimiller.hedgehogs.DataGraph
import net.andimiller.hedgehogs.dag.visitor.DagVisitor.RunMode
import net.andimiller.hedgehogs.dag.visitor.{DagVisitor, SimpleDagVisitor}

/** Runs a plan through hedgehogs and assembles the response.
  *
  * The division of labour is invariant 8: hedgehogs schedules one batch call per node per level; batching *within* a
  * node (ten pets, one call) is `NodeRuntime`'s job.
  *
  * This is the non-durable path: a suspending resolver is an error here, because nothing is persisted to resume from.
  * Use `Durable` for runs that can suspend.
  */
final class Executor[F[_]: Concurrent](
    plan: Plan,
    resolverFor: (String, FieldCoordinate) => Option[Resolver[F]],
    hooks: Hooks[F]
) {

  def run: F[Response] = {
    val rootKeys: Map[NodeId, String] =
      plan.roots.selections.collect { case NodeField(responseKey, _, id, _, _) => id -> responseKey }.toMap

    val graph                         = plan.nodes.foldLeft(DataGraph.empty[NodeId, PlanNode, Unit]) { (acc, node) =>
      node.dependencies.foldLeft(acc.addNode(node.id, node)) { (withNode, dependency) =>
        withNode.addEdge(dependency, node.id, ())
      }
    }

    val visitor = new SimpleDagVisitor[F, NodeId, PlanNode, NodeResult, Unit] {
      def run(id: NodeId, node: PlanNode, inputs: Map[NodeId, NodeResult]): F[NodeResult] =
        resolverFor(node.source, node.field) match {
          case None           =>
            Concurrent[F].raiseError(
              new IllegalStateException(
                s"no resolver registered for ${node.field.render} in source '${node.source}' (plan node ${node.id.value})"
              )
            )
          case Some(resolver) =>
            NodeRuntime
              .run(plan, node, node.dependencies.headOption.flatMap(inputs.get), resolver, rootKeys.get(node.id), hooks)
              .flatMap {
                case NodeOutcome.Completed(result) => result.pure[F]
                case NodeOutcome.Suspended(_)      =>
                  Concurrent[F].raiseError(
                    new IllegalStateException(
                      s"${node.field.render} suspended, but this run is not durable; use Torpor.durable"
                    )
                  )
              }
        }
    }

    DagVisitor
      .runConcurrent(visitor, RunMode.Flow)(graph)
      .map(completed => PlanAssembly.response(plan, completed.nodeMap))
  }
}
