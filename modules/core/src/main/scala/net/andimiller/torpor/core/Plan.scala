package net.andimiller.torpor.core

/** The plan is data: no closures, no effects, no references to resolvers. It can be serialised into a snapshot and
  * replayed, which is why ids are plain strings and argument values are literals (variables are substituted before
  * planning).
  *
  * One node is one batch resolver call per level: invariant 8, "hedgehogs is the stage scheduler, not the field
  * executor". Per-item fan-out happens inside a node, in the executor.
  *
  * `rootType` is the schema type the root selections apply to, which the executor needs for root-level `__typename`.
  */
final case class Plan(rootType: String, roots: SelectionSet, nodes: List[PlanNode]) {
  def node(id: NodeId): Option[PlanNode] = nodes.find(_.id == id)

  /** Longest dependency chain, counting root nodes as level 1. */
  def levels: Int = if (nodes.isEmpty) 0 else nodes.map(_.level).max

  /** Nesting depth of the response, counting root fields as 1 and inline object fields too (unlike `levels`, which only
    * counts batch calls). This is what `maxDepth` means to a user.
    */
  def depth: Int = {
    val byId = nodes.map(node => node.id -> node).toMap

    def nodeDepth(id: NodeId): Int = byId.get(id).map(node => selectionDepth(node.selectionSet)).getOrElse(0)

    def selectionDepth(selectionSet: SelectionSet): Int =
      selectionSet.selections
        .map {
          case InlineField(_, _, selections, _, _) => 1 + selectionDepth(selections)
          case NodeField(_, _, id, _, _)           => 1 + nodeDepth(id)
          case TypeNameField(_, _)                 => 1
        }
        .maxOption
        .getOrElse(0)

    roots.selections
      .map {
        case InlineField(_, _, selections, _, _) => 1 + selectionDepth(selections)
        case NodeField(_, _, id, _, _)           => 1 + nodeDepth(id)
        case TypeNameField(_, _)                 => 1
      }
      .maxOption
      .getOrElse(0)
  }

  /** How many field selections the plan will produce, for `maxFields`. Node selection sets are counted once. */
  def fieldCount: Int = {
    def of(selectionSet: SelectionSet): Int =
      selectionSet.selections.map {
        case InlineField(_, _, selections, _, _) => 1 + of(selections)
        case NodeField(_, _, _, _, _)            => 1
        case TypeNameField(_, _)                 => 1
      }.sum

    of(roots) + nodes.map(node => of(node.selectionSet)).sum
  }
}

final case class PlanNode(
    id: NodeId,
    source: String,
    field: FieldCoordinate,
    args: List[Argument],
    selectionSet: SelectionSet,
    dependencies: List[NodeId],
    /** 1 for a root call; one more than the node that produces the values its keys come from. */
    level: Int
)

final case class NodeId(value: String)

final case class FieldCoordinate(parentType: String, field: String) {
  def render: String = s"$parentType.$field"
}

object FieldCoordinate {

  /** A coordinate whose type names its parent type, its field, the field's arguments and the GraphQL type it returns
    * (with lists and non-null unwrapped), as the schema macro emits them. It is a plain `FieldCoordinate` at runtime;
    * the type exists so the compiler can look things up by parent type and type a root resolver's arguments.
    */
  opaque type Of[Parent <: String, Field <: String, Args, Returns <: String] <: FieldCoordinate = FieldCoordinate
}

final case class Argument(name: String, value: Value)

/** Ordered and normalised: fragments expanded, `@skip`/`@include` applied, same response keys merged and deduped. Two
  * textually different selections that collect to the same fields produce the same set, which is what makes batch
  * grouping correct.
  */
final case class SelectionSet(selections: List[Selection]) {
  def isEmpty: Boolean  = selections.isEmpty
  def nonEmpty: Boolean = selections.nonEmpty
}

object SelectionSet {
  val empty: SelectionSet = SelectionSet(Nil)
}

sealed trait Selection {
  def responseKey: String

  /** Concrete object type names this selection applies to, for inline and named fragments on unions and interfaces. An
    * empty list means every type, so ordinary object selections carry no branching.
    */
  def appliesTo: List[String]
}

/** A field read straight off the value the parent node produced: a scalar, an object field declared by the type's own
  * source, or `__typename`. `selections` is empty for scalars. `tpe` is the field's declared type, which response
  * assembly needs for null bubbling.
  */
final case class InlineField(
    responseKey: String,
    field: String,
    selections: SelectionSet,
    tpe: TypeRef = TypeRef.Named("Unknown"),
    appliesTo: List[String] = Nil
) extends Selection

/** A field that needs its own batch call, provided by the child node. */
final case class NodeField(
    responseKey: String,
    field: String,
    node: NodeId,
    tpe: TypeRef = TypeRef.Named("Unknown"),
    appliesTo: List[String] = Nil
) extends Selection

/** `__typename`, answered from the value when the resolver declared one, otherwise from the static type. */
final case class TypeNameField(responseKey: String, staticName: String) extends Selection {
  def appliesTo: List[String] = Nil
}
