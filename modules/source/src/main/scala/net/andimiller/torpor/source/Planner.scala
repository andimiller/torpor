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
  TypeNameField,
  TypeRef,
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
  * Everything the plan holds is serialisable data: string ids, literal argument values (variables substituted here, SDL
  * defaults filled and values coerced), and selection sets. The snapshot will contain it.
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
          val fields = expand(root, operation.selections, Nil)
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

    /** A field after fragments are expanded and directives applied, still holding raw child selections. `definition` is
      * resolved against the type named by `condition`, which is the innermost fragment's type condition.
      */
    private final case class RawField(
        responseKey: String,
        name: String,
        args: List[QueryArgument],
        selections: List[Selection],
        origin: Origin,
        definition: FieldDef,
        condition: String,
        applies: List[String]
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
      *
      * `inherited` is the set of concrete types the enclosing fragments restrict the selection to; empty means every
      * type the parent can be. A fragment on a union member or interface adds its own restriction.
      */
    private def expand(parent: TypeDef, selections: List[Selection], inherited: List[String]): List[RawField] = {
      val collected = List.newBuilder[RawField]

      selections.foreach {
        case field: FieldSelection =>
          if (included(field.directives)) {
            fieldDefinition(parent, field.name, field.origin).foreach { definition =>
              collected += RawField(
                field.responseKey,
                field.name,
                field.args,
                field.selections,
                field.origin,
                definition,
                parent.name,
                inherited
              )
            }
          }

        case spread: FragmentSpread =>
          if (included(spread.directives)) {
            document.fragment(spread.name) match {
              case None           =>
                error(PlanErrorKind.InvalidField, s"fragment '${spread.name}' is not defined", spread.origin)
              case Some(fragment) =>
                fragmentType(fragment.typeCondition, parent, inherited, spread.origin).foreach {
                  case (condition, applies) =>
                    collected ++= expand(condition, fragment.selections, applies)
                }
            }
          }

        case inline: InlineFragment =>
          if (included(inline.directives)) {
            inline.typeCondition match {
              case None            =>
                collected ++= expand(parent, inline.selections, inherited)
              case Some(condition) =>
                fragmentType(condition, parent, inherited, inline.origin).foreach { case (tpe, applies) =>
                  collected ++= expand(tpe, inline.selections, applies)
                }
            }
          }
      }

      merge(collected.result())
    }

    /** The type a fragment's selections apply to, and the concrete types they are restricted to.
      *
      * `possibleTypes` are the object types a composite type can be at runtime: itself for an object, its members for a
      * union, its implementers for an interface. A fragment is valid when that set overlaps what the parent can be.
      */
    private def fragmentType(
        conditionName: String,
        parent: TypeDef,
        inherited: List[String],
        at: Origin
    ): Option[(TypeDef, List[String])] =
      schema.types.get(conditionName).filter(isComposite) match {
        case None            =>
          error(
            PlanErrorKind.InvalidField,
            s"fragment has type condition '$conditionName', which is not a composite type in the schema",
            at
          )
          None
        case Some(condition) =>
          val parentPossible = if (inherited.nonEmpty) inherited else possibleTypes(parent.name)
          val overlap        = possibleTypes(conditionName).filter(parentPossible.contains)
          if (overlap.isEmpty) {
            error(
              PlanErrorKind.InvalidField,
              s"fragment on '$conditionName' can never apply to '${parent.name}'",
              at
            )
            None
          } else if (condition.name == parent.name) Some((condition, inherited))
          else Some((condition, overlap.sorted))
      }

    /** Object types a composite type can be at runtime. */
    private def possibleTypes(name: String): List[String] =
      schema.types.get(name) match {
        case Some(tpe) if tpe.kind == TypeKind.ObjectType    =>
          List(tpe.name)
        case Some(tpe) if tpe.kind == TypeKind.UnionType     =>
          tpe.unionMembers
        case Some(tpe) if tpe.kind == TypeKind.InterfaceType =>
          schema.types.values.toList
            .filter(possible => possible.kind == TypeKind.ObjectType && possible.interfaces.contains(tpe.name))
            .map(_.name)
            .sorted
        case _                                               => Nil
      }

    private def isComposite(tpe: TypeDef): Boolean =
      tpe.kind == TypeKind.ObjectType || tpe.kind == TypeKind.InterfaceType || tpe.kind == TypeKind.UnionType

    private def merge(fields: List[RawField]): List[RawField] =
      fields.foldLeft(List.empty[RawField]) { (acc, field) =>
        val sameKey = acc.filter(existing =>
          existing.responseKey == field.responseKey && applicable(existing.applies, field.applies)
        )

        sameKey.find(existing => existing.name != field.name || arguments(existing) != arguments(field)) match {
          case Some(existing) =>
            error(
              PlanErrorKind.MergeConflict,
              s"response key '${field.responseKey}' is used for '${existing.name}' and '${field.name}' with different arguments where their type conditions overlap (${existing.origin.render} and ${field.origin.render})",
              field.origin
            )
            acc
          case None           =>
            sameKey.find(_.name == field.name) match {
              case None           => acc :+ field
              case Some(existing) =>
                val applies = mergeApplies(existing.applies, field.applies)
                acc.map { candidate =>
                  if (candidate eq existing)
                    candidate.copy(selections = candidate.selections ++ field.selections, applies = applies)
                  else candidate
                }
            }
        }
      }

    /** Empty means "every type"; disjoint non-empty sets mean the two selections never apply to the same value. */
    private def applicable(first: List[String], second: List[String]): Boolean =
      first.isEmpty || second.isEmpty || first.exists(second.contains)

    private def mergeApplies(first: List[String], second: List[String]): List[String] =
      if (first.isEmpty) second
      else if (second.isEmpty) first
      else first.filter(second.contains).distinct

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
          Some(TypeNameField(raw.responseKey, raw.condition))
        } else {
          val definition = raw.definition
          val childName  = definition.tpe.named
          val args       = planArguments(definition, raw)

          if (isLeaf(childName)) {
            // a leaf added by `extend type` is served by its own resolver, exactly like a composite extension field
            if (atRoot || definition.fromExtension) {
              val id = nextId()
              nodes :+= PlanNode(
                id = id,
                source = definition.origin.source,
                field = FieldCoordinate(raw.condition, raw.name),
                args = args,
                selectionSet = SelectionSet.empty,
                dependencies = parentNode.toList,
                level = level
              )
              Some(NodeField(raw.responseKey, raw.name, id, definition.tpe, raw.applies))
            } else {
              Some(InlineField(raw.responseKey, raw.name, SelectionSet.empty, definition.tpe, raw.applies))
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
                val expanded = expand(child, raw.selections, Nil)
                if (!atRoot && !definition.fromExtension) {
                  val inner = planSelectionSet(child, expanded, level, parentNode, atRoot = false)
                  Some(InlineField(raw.responseKey, raw.name, inner, definition.tpe, raw.applies))
                } else {
                  // One node per selection site, so its arguments are the same for every parent in it. If node sharing
                  // across response keys ever lands, the arguments must join the node's identity.
                  val id    = nextId()
                  val inner = planSelectionSet(child, expanded, level + 1, Some(id), atRoot = false)
                  nodes :+= PlanNode(
                    id = id,
                    source = definition.origin.source,
                    field = FieldCoordinate(raw.condition, raw.name),
                    args = args,
                    selectionSet = inner,
                    dependencies = parentNode.toList,
                    level = level
                  )
                  Some(NodeField(raw.responseKey, raw.name, id, definition.tpe, raw.applies))
                }
            }
          }
        }
      }

      SelectionSet(selections)
    }

    private def fieldDefinition(parent: TypeDef, name: String, at: Origin): Option[FieldDef] =
      if (name == "__typename") Some(FieldDef("__typename", TypeRef.NonNull(TypeRef.Named("String")), Nil, None, at))
      else if (parent.kind == TypeKind.UnionType) {
        error(PlanErrorKind.InvalidField, s"union '${parent.name}' has no field '$name'", at)
        None
      } else
        parent.fields.find(_.name == name) match {
          case None    =>
            error(PlanErrorKind.InvalidField, s"type '${parent.name}' has no field '$name'", at)
            None
          case Some(d) => Some(d)
        }

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

    /** Variables are resolved here, once, so the plan only ever holds literals. */
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

    // ---- arguments --------------------------------------------------------------------------

    /** Fill SDL defaults, substitute variables, and coerce each argument to its declared type, so the plan holds values
      * a resolver's decoder accepts and a bad client value is a request error, not a resolver failure.
      */
    private def planArguments(definition: FieldDef, raw: RawField): List[Argument] = {
      val specs = definition.args.map(spec => spec.name -> spec).toMap

      val providedArgs = raw.args.flatMap { argument =>
        specs.get(argument.name) match {
          case None       =>
            // unknown arguments are a validation error; keep the value so the plan does not silently change
            Some(Argument(argument.name, substitute(argument.value, argument.origin)))
          case Some(spec) =>
            val value = argument.value match {
              case Value.VariableValue(name) =>
                variables
                  .get(name)
                  .orElse(operation.variables.find(_.name == name).flatMap(_.default))
                  .orElse(spec.default) match {
                  case Some(resolved) => Some(resolved)
                  case None           =>
                    error(PlanErrorKind.MissingVariable, s"variable '$$$name' has no value", argument.origin)
                    None
                }
              case other                     =>
                Some(substitute(other, argument.origin))
            }
            value.flatMap(v => coerce(v, spec.tpe, argument.origin, s"${definition.name}.${spec.name}")).map { v =>
              Argument(argument.name, v)
            }
        }
      }

      val defaults = definition.args
        .filter(spec => spec.default.isDefined && !raw.args.exists(_.name == spec.name))
        .flatMap(spec =>
          coerce(spec.default.get, spec.tpe, raw.origin, s"${definition.name}.${spec.name}").map { value =>
            Argument(spec.name, value)
          }
        )

      (providedArgs ++ defaults).sortBy(_.name)
    }

    /** GraphQL input coercion: nullability, lists, the built-in scalars, enums and input objects. A value that does not
      * fit is a plan error, which is a request error per the spec. Custom scalars pass through to the resolver's
      * decoder.
      */
    private def coerce(value: Value, tpe: TypeRef, at: Origin, path: String): Option[Value] =
      tpe match {
        case TypeRef.NonNull(inner) =>
          value match {
            case Value.NullValue =>
              error(PlanErrorKind.InvalidValue, s"$path is non-null, but null was given", at)
              None
            case other           => coerce(other, inner, at, path)
          }

        case TypeRef.ListOf(element) =>
          value match {
            case Value.NullValue     => Some(Value.NullValue)
            case Value.ListValue(vs) =>
              val coerced = vs.map(v => coerce(v, element, at, path).getOrElse(Value.NullValue))
              Some(Value.ListValue(coerced))
            case single              =>
              // a single value where a list is expected is a one-element list, per the spec
              coerce(single, element, at, path).map(v => Value.ListValue(List(v)))
          }

        case TypeRef.Named("Int") =>
          value match {
            case Value.IntValue(v) if v.isValidInt => Some(value)
            case Value.IntValue(_)                 =>
              error(PlanErrorKind.InvalidValue, s"$path is an Int, but the value does not fit in 32 bits", at)
              None
            case other                             =>
              error(PlanErrorKind.InvalidValue, s"$path is an Int, but got $other", at)
              None
          }

        case TypeRef.Named("Float") =>
          value match {
            case Value.FloatValue(_) => Some(value)
            case Value.IntValue(v)   => Some(Value.FloatValue(BigDecimal(v)))
            case other               =>
              error(PlanErrorKind.InvalidValue, s"$path is a Float, but got $other", at)
              None
          }

        case TypeRef.Named("String") =>
          value match {
            case Value.StringValue(_) => Some(value)
            case other                =>
              error(PlanErrorKind.InvalidValue, s"$path is a String, but got $other", at)
              None
          }

        case TypeRef.Named("Boolean") =>
          value match {
            case Value.BooleanValue(_) => Some(value)
            case other                 =>
              error(PlanErrorKind.InvalidValue, s"$path is a Boolean, but got $other", at)
              None
          }

        case TypeRef.Named("ID") =>
          value match {
            case Value.StringValue(_) | Value.IntValue(_) => Some(value)
            case other                                    =>
              error(PlanErrorKind.InvalidValue, s"$path is an ID, but got $other", at)
              None
          }

        case TypeRef.Named(name) =>
          schema.types.get(name) match {
            case Some(tpe) if tpe.kind == TypeKind.EnumType =>
              val allowed = tpe.enumValues.map(_.name).toSet
              value match {
                case Value.EnumValue(v) if allowed(v)   => Some(value)
                case Value.StringValue(v) if allowed(v) => Some(value)
                case other                              =>
                  error(PlanErrorKind.InvalidValue, s"$path is enum '$name', but got $other", at)
                  None
              }

            case Some(tpe) if tpe.kind == TypeKind.InputObjectType =>
              coerceInputObject(value, tpe, at, path)

            case _ =>
              // custom scalar, or a type the validator will report
              Some(value)
          }
      }

    private def coerceInputObject(
        value: Value,
        tpe: TypeDef,
        at: Origin,
        path: String
    ): Option[Value] =
      value match {
        case Value.ObjectValue(provided) =>
          val givenFields = provided.toMap
          val known       = tpe.fields.map(_.name).toSet
          givenFields.keys
            .filterNot(known)
            .foreach(field => error(PlanErrorKind.InvalidValue, s"$path has no field '$field'", at))
          val fields      = tpe.fields.map { field =>
            givenFields.get(field.name) match {
              case Some(v) => coerce(v, field.tpe, at, s"$path.${field.name}")
              case None    =>
                field.defaultValue.orElse(if (field.tpe.isNonNull) None else Some(Value.NullValue)) match {
                  case Some(v) => coerce(v, field.tpe, at, s"$path.${field.name}")
                  case None    =>
                    error(PlanErrorKind.InvalidValue, s"$path requires field '${field.name}'", at)
                    None
                }
            }
          }
          if (fields.contains(None)) None
          else
            Some(Value.ObjectValue(tpe.fields.zip(fields).collect { case (field, Some(v)) =>
              field.name -> v
            }))
        case other                       =>
          error(PlanErrorKind.InvalidValue, s"$path is input object '${tpe.name}', but got $other", at)
          None
      }

    private def isLeaf(name: String): Boolean =
      builtinScalars(name) || schema.types
        .get(name)
        .exists(t => t.kind == TypeKind.ScalarType || t.kind == TypeKind.EnumType)
  }
}
