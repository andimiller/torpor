package net.andimiller.torpor.source

import cats.data.NonEmptyChain
// core's Argument/Value are the plan's; schema's wildcard supplies the query AST, whose `Selection` and
// `QueryArgument` are distinct types. Named imports win over the wildcard.
import net.andimiller.torpor.core.{
  Argument,
  FieldCoordinate,
  InlineField,
  NodeField,
  NodeId,
  Plan,
  PlanNode,
  SelectionSet,
  Value
}
import net.andimiller.torpor.schema.*
import net.andimiller.torpor.schema.Argument as QueryArgument

enum PlanErrorKind(val label: String) {
  case MissingRoot extends PlanErrorKind("missing root type")
  case InvalidField extends PlanErrorKind("invalid field")
  case MissingVariable extends PlanErrorKind("missing variable")
  case MergeConflict extends PlanErrorKind("merge conflict")
  case InvalidValue extends PlanErrorKind("invalid value")
  case LimitExceeded extends PlanErrorKind("limit exceeded")
  case Unsupported extends PlanErrorKind("unsupported")
}

final case class PlanError(kind: PlanErrorKind, detail: String, origin: Origin) {
  def render: String = s"${origin.render}: ${kind.label}: $detail"
}

/** Query + merged schema → plan. Pure: no effects, no resolvers, no I/O.
  *
  * Field ownership comes from the merged schema: a field declared by an `extend type` needs its own batch call, so it
  * becomes a node; a field declared in the type definition is already on the value the parent node produced, so it is
  * read inline. Root fields always become nodes, because there is no parent value to read them from.
  *
  * Everything the plan holds is serialisable data: string ids, literal argument values (variables substituted here),
  * and selection sets. The snapshot will contain it.
  */
object Planner {

  def build(
      schema: Schema,
      validated: Validated,
      variables: Map[String, Value] = Map.empty,
      limits: Limits = Limits.none
  ): Either[NonEmptyChain[PlanError], Plan] =
    new Builder(schema, validated, variables, limits).run()

  private val builtinScalars = Set("Int", "Float", "String", "Boolean", "ID")

  /** Checked on the plan, not on the query text: the plan is what will actually run. */
  final case class Limits(maxDepth: Int = Int.MaxValue, maxFields: Int = Int.MaxValue)

  object Limits {
    val none: Limits = Limits()
  }

  private final class Builder(
      schema: Schema,
      validated: Validated,
      variables: Map[String, Value],
      limits: Limits
  ) {
    import validated.{document, operation}

    private var nodes   = List.empty[PlanNode]
    private var counter = 0
    private val errors  = Vector.newBuilder[PlanError]

    def run(): Either[NonEmptyChain[PlanError], Plan] = {
      val rootName = operation.kind match {
        case OperationKind.Query        => schema.query
        case OperationKind.Mutation     => schema.mutation
        case OperationKind.Subscription => schema.subscription
      }

      val (rootTypeName, roots) = rootName.flatMap(schema.types.get) match {
        case None       =>
          error(PlanErrorKind.MissingRoot, s"the schema has no ${operation.kind.label} root type", operation.origin)
          ("", SelectionSet.empty)
        case Some(root) =>
          val fields = expand(root.name, operation.selections)
          (root.name, planSelectionSet(root, fields, level = 1, parentNode = None, atRoot = true))
      }

      val plan = Plan(rootTypeName, roots, nodes.sortBy(node => idOrder(node.id)).toList)

      if (plan.depth > limits.maxDepth) {
        error(
          PlanErrorKind.LimitExceeded,
          s"query depth ${plan.depth} exceeds maxDepth ${limits.maxDepth}",
          operation.origin
        )
      }
      if (plan.fieldCount > limits.maxFields) {
        error(
          PlanErrorKind.LimitExceeded,
          s"query has ${plan.fieldCount} fields, which exceeds maxFields ${limits.maxFields}",
          operation.origin
        )
      }

      NonEmptyChain.fromSeq(errors.result()) match {
        case Some(problems) => Left(problems)
        case None           => Right(plan)
      }
    }

    private def error(kind: PlanErrorKind, detail: String, at: Origin): Unit =
      errors += PlanError(kind, detail, at)

    /** A field after fragments are expanded and directives applied, still holding raw child selections. */
    private final case class RawField(
        responseKey: String,
        name: String,
        args: List[QueryArgument],
        selections: List[Selection],
        origin: Origin
    )

    private def nextId(): NodeId = {
      val id = NodeId(s"n$counter")
      counter += 1
      id
    }

    private def idOrder(id: NodeId): Long =
      id.value.drop(1).toLongOption.getOrElse(0L)

    /** Expand fragments, evaluate `@skip`/`@include`, then merge selections that share a response key. This is the
      * normalisation the notes insist on: batching keys are selection sets, so two textually different selections that
      * collect to the same fields must produce the same set.
      */
    private def expand(parentType: String, selections: List[Selection]): List[RawField] = {
      val collected = List.newBuilder[RawField]

      selections.foreach {
        case field: FieldSelection =>
          if (included(field.directives)) {
            collected += RawField(field.responseKey, field.name, field.args, field.selections, field.origin)
          }

        case spread: FragmentSpread =>
          if (included(spread.directives)) {
            document.fragment(spread.name) match {
              case None                                                   =>
                error(PlanErrorKind.InvalidField, s"fragment '${spread.name}' is not defined", spread.origin)
              case Some(fragment) if fragment.typeCondition != parentType =>
                error(
                  PlanErrorKind.Unsupported,
                  s"fragment '${spread.name}' narrows '$parentType' to '${fragment.typeCondition}'; type conditions are not implemented until M1",
                  spread.origin
                )
              case Some(fragment)                                         =>
                collected ++= expand(parentType, fragment.selections)
            }
          }

        case inline: InlineFragment =>
          if (included(inline.directives)) {
            inline.typeCondition match {
              case Some(condition) if condition != parentType =>
                error(
                  PlanErrorKind.Unsupported,
                  s"inline fragment narrows '$parentType' to '$condition'; type conditions are not implemented until M1",
                  inline.origin
                )
              case _                                          =>
                collected ++= expand(parentType, inline.selections)
            }
          }
      }

      merge(collected.result())
    }

    private def merge(fields: List[RawField]): List[RawField] =
      fields.foldLeft(List.empty[RawField]) { (acc, field) =>
        acc.indexWhere(_.responseKey == field.responseKey) match {
          case -1 => acc :+ field
          case i  =>
            val existing = acc(i)
            if (existing.name != field.name) {
              error(
                PlanErrorKind.MergeConflict,
                s"response key '${field.responseKey}' is used for both '${existing.name}' and '${field.name}' after fragment expansion (${existing.origin.render} and ${field.origin.render})",
                field.origin
              )
              acc
            } else if (arguments(existing) != arguments(field)) {
              error(
                PlanErrorKind.MergeConflict,
                s"response key '${field.responseKey}' is selected with different arguments after fragment expansion (${existing.origin.render} and ${field.origin.render})",
                field.origin
              )
              acc
            } else {
              acc.updated(i, existing.copy(selections = existing.selections ++ field.selections))
            }
        }
      }

    private def arguments(field: RawField): List[(String, Value)] =
      field.args.sortBy(_.name).map(a => a.name -> substitute(a.value, a.origin))

    private def planSelectionSet(
        parent: TypeDef,
        fields: List[RawField],
        level: Int,
        parentNode: Option[NodeId],
        atRoot: Boolean
    ): SelectionSet = {
      val selections = fields.flatMap { raw =>
        if (raw.name == "__typename") {
          // Needs the static type at each selection level, which the plan does not carry yet. Failing loudly beats
          // returning null for a field every Apollo client asks for.
          error(PlanErrorKind.Unsupported, "__typename is not implemented until M1", raw.origin)
          None
        } else {
          fieldDef(parent, raw.name) match {
            case None =>
              error(PlanErrorKind.InvalidField, s"type '${parent.name}' has no field '${raw.name}'", raw.origin)
              None

            case Some(definition) =>
              val args      = raw.args.map(a => Argument(a.name, substitute(a.value, a.origin)))
              val childName = definition.tpe.named

              if (isLeaf(childName)) {
                if (atRoot) {
                  val id = nextId()
                  nodes :+= PlanNode(
                    id = id,
                    source = definition.origin.source,
                    field = FieldCoordinate(parent.name, raw.name),
                    args = args,
                    selectionSet = SelectionSet.empty,
                    dependencies = parentNode.toList,
                    level = level
                  )
                  Some(NodeField(raw.responseKey, raw.name, id))
                } else {
                  Some(InlineField(raw.responseKey, raw.name, SelectionSet.empty))
                }
              } else {
                schema.types.get(childName) match {
                  case None        =>
                    error(
                      PlanErrorKind.InvalidField,
                      s"field '${parent.name}.${raw.name}' has type '$childName', which is not defined",
                      raw.origin
                    )
                    None
                  case Some(child) =>
                    val expanded = expand(child.name, raw.selections)
                    if (!atRoot && !definition.fromExtension) {
                      val inner = planSelectionSet(child, expanded, level, parentNode, atRoot = false)
                      Some(InlineField(raw.responseKey, raw.name, inner))
                    } else {
                      val id    = nextId()
                      val inner = planSelectionSet(child, expanded, level + 1, Some(id), atRoot = false)
                      nodes :+= PlanNode(
                        id = id,
                        source = definition.origin.source,
                        field = FieldCoordinate(parent.name, raw.name),
                        args = args,
                        selectionSet = inner,
                        dependencies = parentNode.toList,
                        level = level
                      )
                      Some(NodeField(raw.responseKey, raw.name, id))
                    }
                }
              }
          }
        }
      }

      SelectionSet(selections)
    }

    private def fieldDef(parent: TypeDef, name: String): Option[FieldDef] =
      parent.fields.find(_.name == name)

    private def included(directives: List[Directive]): Boolean =
      directives.forall { directive =>
        directive.name match {
          case "skip"    =>
            booleanArgument(directive) match {
              case Some(true) => false
              case _          => true
            }
          case "include" =>
            booleanArgument(directive) match {
              case Some(false) => false
              case _           => true
            }
          case _         => true
        }
      }

    private def booleanArgument(directive: Directive): Option[Boolean] =
      directive.args.find(_.name == "if") match {
        case None      => None
        case Some(arg) =>
          substitute(arg.value, arg.origin) match {
            case Value.BooleanValue(value) => Some(value)
            case other                     =>
              error(
                PlanErrorKind.InvalidValue,
                s"directive '@${directive.name}' expects a boolean 'if', got $other",
                arg.origin
              )
              None
          }
      }

    /** Variables are resolved here, once, so the plan only ever holds literals. Values are not coerced (5.6.1 is not
      * implemented); a value is used exactly as the client sent it.
      */
    private def substitute(value: Value, at: Origin): Value =
      value match {
        case Value.VariableValue(name) =>
          variables.get(name).orElse(operation.variables.find(_.name == name).flatMap(_.default)) match {
            case Some(resolved) => resolved
            case None           =>
              error(PlanErrorKind.MissingVariable, s"variable '$$$name' has no value", at)
              Value.NullValue
          }
        case Value.ListValue(values)   => Value.ListValue(values.map(substitute(_, at)))
        case Value.ObjectValue(fields) => Value.ObjectValue(fields.map { case (name, v) => name -> substitute(v, at) })
        case other                     => other
      }

    private def isLeaf(name: String): Boolean =
      builtinScalars(name) || schema.types
        .get(name)
        .exists(t => t.kind == TypeKind.ScalarType || t.kind == TypeKind.EnumType)
  }
}
