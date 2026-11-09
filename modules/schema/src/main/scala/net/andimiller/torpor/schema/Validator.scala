package net.andimiller.torpor.schema

import net.andimiller.torpor.core.Value

import scala.collection.mutable

/** The M0 validation subset.
  *
  * Everything here is either needed by the planner (a selection set it can walk without surprises) or cheap and
  * protective (cycles, duplicate fragment names). Rules that only shape the *response* (5.3.2's cross-fragment merging,
  * 5.6.1's value coercion, 5.5.1.1's variable positions) are deliberately absent; SUPPORT.md lists the gap.
  */
private[schema] object Validator {

  private type Err = (ValidationRule, String, Origin) => Unit

  private enum Location {
    case FieldLocation, FragmentSpreadLocation, InlineFragmentLocation, OperationLocation, FragmentDefinitionLocation
  }

  private val builtinScalars = Set("Int", "Float", "String", "Boolean", "ID")

  private val directiveLocations: Map[String, Set[Location]] = Map(
    "skip"    -> Set(Location.FieldLocation, Location.FragmentSpreadLocation, Location.InlineFragmentLocation),
    "include" -> Set(Location.FieldLocation, Location.FragmentSpreadLocation, Location.InlineFragmentLocation)
  )

  private val unimplementedDirectives = Set("defer", "stream")

  def validate(schema: Schema, document: ExecutableDocument): List[ValidationError] = {
    val errors                                                      = Vector.newBuilder[ValidationError]
    def err(rule: ValidationRule, detail: String, at: Origin): Unit =
      errors += ValidationError(rule, detail, at)

    checkOperationNames(document, err)
    checkFragmentNames(document, err)
    checkFragmentConditions(schema, document, err)
    checkFragmentCycles(document, err)
    checkVariableDefinitions(schema, document, err)

    document.operations.foreach { operation =>
      checkDirectives(operation.directives, Location.OperationLocation, err)
      if (operation.kind == OperationKind.Subscription) {
        // Unimplemented, not invalid: say so before the missing-root check, which would be a misleading error.
        err(ValidationRule.Unsupported, "subscriptions are not implemented yet", operation.origin)
      } else
        rootTypeName(schema, operation.kind) match {
          case None           =>
            err(ValidationRule.OperationType, s"the schema has no ${operation.kind.label} root type", operation.origin)
          case Some(rootName) =>
            schema.types.get(rootName) match {
              case None         =>
                err(
                  ValidationRule.OperationType,
                  s"root ${operation.kind.label} type '$rootName' is not defined",
                  operation.origin
                )
              case Some(parent) => walk(schema, document, parent, operation.selections, err)
            }
        }
    }

    document.fragments.foreach { fragment =>
      checkDirectives(fragment.directives, Location.FragmentDefinitionLocation, err)
      // A bad type condition is reported by checkFragmentConditions; walking it here would duplicate that error.
      compositeType(schema, fragment.typeCondition).foreach { parent =>
        walk(schema, document, parent, fragment.selections, err)
      }
    }

    checkUndefinedVariables(document, err)

    errors.result().toList
  }

  private def walk(
      schema: Schema,
      document: ExecutableDocument,
      parent: TypeDef,
      selections: List[Selection],
      err: Err
  ): Unit = {
    checkFieldMerging(parent, selections, err)

    selections.foreach {
      case field: FieldSelection =>
        checkDirectives(field.directives, Location.FieldLocation, err)
        if (field.name == "__schema" || field.name == "__type") {
          err(ValidationRule.Unsupported, s"introspection field '${field.name}' is not implemented yet", field.origin)
        } else {
          fieldDef(parent, field.name) match {
            case None             =>
              err(ValidationRule.FieldSelections, s"type '${parent.name}' has no field '${field.name}'", field.origin)
            case Some(definition) =>
              checkArgs(s"field '${parent.name}.${field.name}'", field.args, definition.args, field.origin, err)
              val childName = definition.tpe.named
              if (isLeaf(schema, childName)) {
                if (field.selections.nonEmpty) {
                  err(
                    ValidationRule.LeafFieldSelections,
                    s"field '${parent.name}.${field.name}' has type '$childName', which is a leaf, so it must not have a selection set",
                    field.origin
                  )
                }
              } else {
                schema.types.get(childName) match {
                  case None                                   =>
                    err(
                      ValidationRule.FieldSelections,
                      s"field '${parent.name}.${field.name}' has type '$childName', which is not defined",
                      field.origin
                    )
                  case Some(child) if isComposite(child.kind) =>
                    if (field.selections.isEmpty) {
                      err(
                        ValidationRule.LeafFieldSelections,
                        s"field '${parent.name}.${field.name}' has type '$childName', which is composite, so it must have a selection set",
                        field.origin
                      )
                    } else {
                      walk(schema, document, child, field.selections, err)
                    }
                  case Some(child)                            =>
                    err(
                      ValidationRule.LeafFieldSelections,
                      s"field '${parent.name}.${field.name}' has type '$childName', which is a ${child.kind.label}, so it must not have a selection set",
                      field.origin
                    )
                }
              }
          }
        }

      case spread: FragmentSpread =>
        checkDirectives(spread.directives, Location.FragmentSpreadLocation, err)
        if (document.fragment(spread.name).isEmpty) {
          err(ValidationRule.FragmentTypeExistence, s"fragment '${spread.name}' is not defined", spread.origin)
        }

      case inline: InlineFragment =>
        checkDirectives(inline.directives, Location.InlineFragmentLocation, err)
        val condition = inline.typeCondition match {
          case None       => Some(parent)
          case Some(name) =>
            kindOf(schema, name) match {
              case None                             =>
                err(
                  ValidationRule.FragmentTypeExistence,
                  s"inline fragment has type condition '$name', which is not defined",
                  inline.origin
                )
                None
              case Some(kind) if !isComposite(kind) =>
                err(
                  ValidationRule.FragmentTypeExistence,
                  s"inline fragment has type condition '$name', which is a ${kind.label}, not a composite type",
                  inline.origin
                )
                None
              case Some(_)                          => compositeType(schema, name)
            }
        }
        condition.foreach(tpe => walk(schema, document, tpe, inline.selections, err))
    }
  }

  private def checkArgs(
      owner: String,
      args: List[Argument],
      definitions: List[InputValueDef],
      at: Origin,
      err: Err
  ): Unit = {
    args.groupBy(_.name).foreach { case (name, occurrences) =>
      occurrences
        .drop(1)
        .foreach(a =>
          err(ValidationRule.ArgumentUniqueness, s"$owner has argument '$name' ${occurrences.size} times", a.origin)
        )
    }
    args.foreach { arg =>
      if (!definitions.exists(_.name == arg.name)) {
        err(ValidationRule.ArgumentNames, s"$owner has no argument '${arg.name}'", arg.origin)
      }
    }
    definitions.filter(d => d.tpe.isNonNull && !d.hasDefault).foreach { required =>
      if (!args.exists(_.name == required.name)) {
        err(ValidationRule.RequiredArguments, s"$owner requires argument '${required.name}'", at)
      }
    }
  }

  private def checkDirectives(directives: List[Directive], location: Location, err: Err): Unit = {
    directives.groupBy(_.name).foreach { case (name, occurrences) =>
      occurrences
        .drop(1)
        .foreach(d =>
          err(
            ValidationRule.UniqueDirectivesPerLocation,
            s"directive '@$name' is applied ${occurrences.size} times to one location",
            d.origin
          )
        )
    }
    directives.foreach { directive =>
      directiveLocations.get(directive.name) match {
        case None if unimplementedDirectives(directive.name) =>
          err(ValidationRule.Unsupported, s"directive '@${directive.name}' is not implemented yet", directive.origin)
        case None                                            =>
          err(ValidationRule.DirectiveNames, s"directive '@${directive.name}' is not defined", directive.origin)
        case Some(allowed)                                   =>
          if (!allowed(location)) {
            err(
              ValidationRule.DirectiveLocations,
              s"directive '@${directive.name}' is not allowed on a ${location.toString}",
              directive.origin
            )
          }
          checkArgs(
            s"directive '@${directive.name}'",
            directive.args,
            List(InputValueDef("if", TypeRef.NonNull(TypeRef.Named("Boolean")))),
            directive.origin,
            err
          )
      }
    }
  }

  /** 5.3.2, the part that matters before planning: one response key, one field, one set of arguments. Cross-fragment
    * merging and return-type compatibility are not implemented (see SUPPORT.md).
    */
  private def checkFieldMerging(parent: TypeDef, selections: List[Selection], err: Err): Unit = {
    val fields = selections.collect { case f: FieldSelection => f }
    fields.groupBy(_.responseKey).foreach { case (key, occurrences) =>
      val first = occurrences.head
      occurrences.tail.foreach { other =>
        if (first.name != other.name) {
          err(
            ValidationRule.FieldSelectionMerging,
            s"response key '$key' is used for both '${first.name}' and '${other.name}' on '${parent.name}'",
            other.origin
          )
        } else if (argumentsOf(first) != argumentsOf(other)) {
          err(
            ValidationRule.FieldSelectionMerging,
            s"response key '$key' is selected twice with different arguments on '${parent.name}'",
            other.origin
          )
        }
      }
    }
  }

  private def argumentsOf(field: FieldSelection): List[(String, Value)] =
    field.args.sortBy(_.name).map(a => a.name -> a.value)

  private def checkOperationNames(document: ExecutableDocument, err: Err): Unit = {
    val named = document.operations.flatMap(op => op.name.map(_ -> op))
    named.groupBy(_._1).foreach { case (name, occurrences) =>
      occurrences.drop(1).foreach { case (_, op) =>
        err(
          ValidationRule.OperationNameUniqueness,
          s"operation '$name' is defined ${occurrences.size} times",
          op.origin
        )
      }
    }
    if (document.operations.size > 1) {
      document.operations.filter(_.name.isEmpty).foreach { op =>
        err(
          ValidationRule.LoneAnonymousOperation,
          "an anonymous operation must be the only operation in the document",
          op.origin
        )
      }
    }
  }

  private def checkFragmentNames(document: ExecutableDocument, err: Err): Unit = {
    document.fragments.groupBy(_.name).foreach { case (name, occurrences) =>
      occurrences
        .drop(1)
        .foreach(f =>
          err(ValidationRule.FragmentNameUniqueness, s"fragment '$name' is defined ${occurrences.size} times", f.origin)
        )
    }
  }

  private def checkFragmentConditions(schema: Schema, document: ExecutableDocument, err: Err): Unit =
    document.fragments.foreach { fragment =>
      kindOf(schema, fragment.typeCondition) match {
        case None                             =>
          err(
            ValidationRule.FragmentTypeExistence,
            s"fragment '${fragment.name}' has type condition '${fragment.typeCondition}', which is not defined",
            fragment.origin
          )
        case Some(kind) if !isComposite(kind) =>
          err(
            ValidationRule.FragmentTypeExistence,
            s"fragment '${fragment.name}' has type condition '${fragment.typeCondition}', which is a ${kind.label}, not a composite type",
            fragment.origin
          )
        case Some(_)                          => ()
      }
    }

  private def checkFragmentCycles(document: ExecutableDocument, err: Err): Unit = {
    val edges    = document.fragments.map(f => f.name -> spreadsIn(f.selections).distinct).toMap
    val byName   = document.fragments.map(f => f.name -> f).toMap
    val reported = mutable.Set.empty[Set[String]]

    edges.keys.toList.sorted.foreach { start =>
      def dfs(node: String, path: List[String]): Unit =
        edges.getOrElse(node, Nil).foreach { next =>
          if (next == start) {
            val cycle = (start :: path).toSet
            if (reported.add(cycle)) {
              err(
                ValidationRule.FragmentCycles,
                s"fragments form a cycle: ${(start :: path).reverse.mkString(" -> ")} -> $start",
                byName(start).origin
              )
            }
          } else if (!path.contains(next)) {
            dfs(next, next :: path)
          }
        }
      dfs(start, Nil)
    }
  }

  private def checkVariableDefinitions(schema: Schema, document: ExecutableDocument, err: Err): Unit =
    document.operations.foreach { operation =>
      operation.variables.groupBy(_.name).foreach { case (name, occurrences) =>
        occurrences
          .drop(1)
          .foreach(v =>
            err(ValidationRule.VariableUniqueness, s"variable '$$$name' is defined ${occurrences.size} times", v.origin)
          )
      }
      operation.variables.foreach { variable =>
        val name = variable.tpe.named
        if (!builtinScalars(name)) {
          schema.types.get(name) match {
            case None                            =>
              err(
                ValidationRule.VariablesAreInputTypes,
                s"variable '$$${variable.name}' has type '$name', which is not defined",
                variable.origin
              )
            case Some(tpe) if !isInput(tpe.kind) =>
              err(
                ValidationRule.VariablesAreInputTypes,
                s"variable '$$${variable.name}' has type '$name', which is a ${tpe.kind.label}, not an input type",
                variable.origin
              )
            case Some(_)                         => ()
          }
        }
      }
    }

  private def checkUndefinedVariables(document: ExecutableDocument, err: Err): Unit =
    document.operations.foreach { operation =>
      val defined = operation.variables.map(_.name).toSet
      val label   = operation.name.fold("the anonymous operation")(n => s"operation '$n'")
      usages(document, operation.selections).foreach { case (name, at) =>
        if (!defined(name)) {
          err(ValidationRule.UndefinedVariables, s"variable '$$$name' is used but not defined by $label", at)
        }
      }
    }

  private def usages(document: ExecutableDocument, selections: List[Selection]): List[(String, Origin)] = {
    val direct      = argumentsIn(selections)
    val inFragments = reachableFragments(document, selections).flatMap(f => argumentsIn(f.selections))
    (direct ++ inFragments).flatMap { case (value, at) => variablesIn(value).map(_ -> at) }
  }

  private def argumentsIn(selections: List[Selection]): List[(Value, Origin)] =
    selections.flatMap {
      case field: FieldSelection  =>
        field.args.map(a => a.value -> a.origin) ++
          field.directives.flatMap(_.args.map(a => a.value -> a.origin)) ++
          argumentsIn(field.selections)
      case spread: FragmentSpread =>
        spread.directives.flatMap(_.args.map(a => a.value -> a.origin))
      case inline: InlineFragment =>
        inline.directives.flatMap(_.args.map(a => a.value -> a.origin)) ++ argumentsIn(inline.selections)
    }

  private def variablesIn(value: Value): List[String] =
    value match {
      case Value.VariableValue(name) => List(name)
      case Value.ListValue(values)   => values.flatMap(variablesIn)
      case Value.ObjectValue(fields) => fields.flatMap(f => variablesIn(f._2))
      case _                         => Nil
    }

  private def spreadsIn(selections: List[Selection]): List[String] =
    selections.flatMap {
      case field: FieldSelection  => spreadsIn(field.selections)
      case spread: FragmentSpread => List(spread.name)
      case inline: InlineFragment => spreadsIn(inline.selections)
    }

  private def reachableFragments(
      document: ExecutableDocument,
      selections: List[Selection]
  ): List[FragmentDefinition] = {
    val seen                                                   = mutable.Set.empty[String]
    def go(current: List[Selection]): List[FragmentDefinition] =
      spreadsIn(current).flatMap { name =>
        if (!seen.add(name)) Nil
        else
          document.fragment(name) match {
            case None    => Nil
            case Some(f) => f :: go(f.selections)
          }
      }
    go(selections)
  }

  private def fieldDef(parent: TypeDef, name: String): Option[FieldDef] =
    if (name == "__typename") {
      Some(FieldDef("__typename", TypeRef.NonNull(TypeRef.Named("String")), Nil, None, parent.origin))
    } else if (parent.kind == TypeKind.UnionType) {
      None // unions have no fields besides __typename
    } else {
      parent.fields.find(_.name == name)
    }

  private def rootTypeName(schema: Schema, kind: OperationKind): Option[String] =
    kind match {
      case OperationKind.Query        => schema.query
      case OperationKind.Mutation     => schema.mutation
      case OperationKind.Subscription => schema.subscription
    }

  /** Built-in scalars are not in the merged type map, so they are resolved here rather than treated as unknown. */
  private def kindOf(schema: Schema, name: String): Option[TypeKind] =
    schema.types.get(name).map(_.kind).orElse(if (builtinScalars(name)) Some(TypeKind.ScalarType) else None)

  private def compositeType(schema: Schema, name: String): Option[TypeDef] =
    schema.types.get(name).filter(t => isComposite(t.kind))

  private def isLeaf(schema: Schema, name: String): Boolean =
    builtinScalars(name) || schema.types
      .get(name)
      .exists(t => t.kind == TypeKind.ScalarType || t.kind == TypeKind.EnumType)

  private def isComposite(kind: TypeKind): Boolean =
    kind match {
      case TypeKind.ObjectType | TypeKind.InterfaceType | TypeKind.UnionType => true
      case _                                                                 => false
    }

  private def isInput(kind: TypeKind): Boolean =
    kind match {
      case TypeKind.ScalarType | TypeKind.EnumType | TypeKind.InputObjectType => true
      case _                                                                  => false
    }
}
