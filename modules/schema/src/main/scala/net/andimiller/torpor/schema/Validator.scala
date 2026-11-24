package net.andimiller.torpor.schema

import net.andimiller.torpor.core.Value

import scala.collection.mutable

/** The validation rules torpor implements: SUPPORT.md lists them against the specification's section 5.
  *
  * Everything a client can get wrong in a document is meant to be found here rather than at execution, because the same
  * validator runs when `torpor-client` compiles an operation.
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
      checkDirectives(schema, operation.directives, Location.OperationLocation, err)
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
              case Some(parent) =>
                walk(schema, document, parent, operation.selections, err)
                checkFieldsCanMerge(schema, document, parent, operation.selections, err)
            }
        }
    }

    document.fragments.foreach { fragment =>
      checkDirectives(schema, fragment.directives, Location.FragmentDefinitionLocation, err)
      // A bad type condition is reported by checkFragmentConditions; walking it here would duplicate that error.
      compositeType(schema, fragment.typeCondition).foreach { parent =>
        walk(schema, document, parent, fragment.selections, err)
        checkFieldsCanMerge(schema, document, parent, fragment.selections, err)
      }
    }

    checkVariableUsages(schema, document, err)

    // a fragment's problems are found both on their own and through every operation that spreads it
    errors.result().distinct.toList
  }

  private def walk(
      schema: Schema,
      document: ExecutableDocument,
      parent: TypeDef,
      selections: List[Selection],
      err: Err
  ): Unit =
    selections.foreach {
      case field: FieldSelection =>
        checkDirectives(schema, field.directives, Location.FieldLocation, err)
        if (field.name == "__schema" || field.name == "__type") {
          err(ValidationRule.Unsupported, s"introspection field '${field.name}' is not implemented yet", field.origin)
        } else {
          fieldDef(parent, field.name) match {
            case None             =>
              err(ValidationRule.FieldSelections, s"type '${parent.name}' has no field '${field.name}'", field.origin)
            case Some(definition) =>
              checkArgs(schema, s"field '${parent.name}.${field.name}'", field.args, definition.args, field.origin, err)
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
        checkDirectives(schema, spread.directives, Location.FragmentSpreadLocation, err)
        document.fragment(spread.name) match {
          case None           =>
            err(ValidationRule.FragmentTypeExistence, s"fragment '${spread.name}' is not defined", spread.origin)
          case Some(fragment) =>
            // a condition that is not a composite type is reported where the fragment is defined
            if (
              compositeType(schema, fragment.typeCondition).isDefined && !canSpread(
                schema,
                parent,
                fragment.typeCondition
              )
            ) {
              err(
                ValidationRule.PossibleFragmentSpreads,
                s"fragment '${spread.name}' on '${fragment.typeCondition}' can never apply to '${parent.name}'",
                spread.origin
              )
            }
        }

      case inline: InlineFragment =>
        checkDirectives(schema, inline.directives, Location.InlineFragmentLocation, err)
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
              case Some(_)                          =>
                if (!canSpread(schema, parent, name)) {
                  err(
                    ValidationRule.PossibleFragmentSpreads,
                    s"inline fragment on '$name' can never apply to '${parent.name}'",
                    inline.origin
                  )
                }
                compositeType(schema, name)
            }
        }
        condition.foreach(tpe => walk(schema, document, tpe, inline.selections, err))
    }

  /** 5.5.2.3: a fragment can apply only if its condition and the parent can be the same object type. */
  private def canSpread(schema: Schema, parent: TypeDef, condition: String): Boolean =
    possibleTypes(schema, parent.name).exists(possibleTypes(schema, condition))

  /** The object types a composite type can be at runtime. */
  private def possibleTypes(schema: Schema, name: String): Set[String] =
    schema.types.get(name) match {
      case Some(tpe) if tpe.kind == TypeKind.ObjectType    => Set(tpe.name)
      case Some(tpe) if tpe.kind == TypeKind.UnionType     => tpe.unionMembers.toSet
      case Some(tpe) if tpe.kind == TypeKind.InterfaceType =>
        schema.types.values
          .filter(t => t.kind == TypeKind.ObjectType && t.interfaces.contains(tpe.name))
          .map(_.name)
          .toSet
      case _                                               => Set.empty
    }

  private def checkArgs(
      schema: Schema,
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
      definitions.find(_.name == arg.name) match {
        case None             => err(ValidationRule.ArgumentNames, s"$owner has no argument '${arg.name}'", arg.origin)
        case Some(definition) =>
          checkValue(schema, definition.tpe, arg.value, s"$owner argument '${arg.name}'", arg.origin, err)
      }
    }
    definitions.filter(d => d.tpe.isNonNull && !d.hasDefault).foreach { required =>
      if (!args.exists(_.name == required.name)) {
        err(ValidationRule.RequiredArguments, s"$owner requires argument '${required.name}'", at)
      }
    }
  }

  private def checkDirectives(schema: Schema, directives: List[Directive], location: Location, err: Err): Unit = {
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
          checkArgs(schema, s"directive '@${directive.name}'", directive.args, conditionArgs, directive.origin, err)
      }
    }
  }

  /** 5.3.2 Field Selection Merging, as the specification's `FieldsInSetCanMerge`: every pair of selections that share a
    * response key, through fragments, must have the same response shape, and where they can apply to the same object
    * they must be the same field with the same arguments, with their selection sets merging in turn.
    */
  private def checkFieldsCanMerge(
      schema: Schema,
      document: ExecutableDocument,
      parent: TypeDef,
      selections: List[Selection],
      err: Err
  ): Unit =
    collectFields(schema, document, parent, selections, Set.empty).groupBy(_.field.responseKey).foreach {
      case (key, occurrences) =>
        occurrences.tails.foreach {
          case first :: rest => rest.foreach(other => checkPairCanMerge(schema, document, key, first, other, err))
          case Nil           => ()
        }
        // a key selected once still has a selection set of its own to check
        occurrences match {
          case only :: Nil =>
            beneath(schema, only).foreach(checkFieldsCanMerge(schema, document, _, only.field.selections, err))
          case _           => ()
        }
    }

  private def checkPairCanMerge(
      schema: Schema,
      document: ExecutableDocument,
      key: String,
      first: Collected,
      other: Collected,
      err: Err
  ): Unit = {
    (first.definition, other.definition) match {
      case (Some(a), Some(b))
          if !sameResponseShape(schema, document, a.tpe, first.field.selections, b.tpe, other.field.selections) =>
        err(
          ValidationRule.FieldSelectionMerging,
          s"response key '$key' is '${a.tpe.render}' on '${first.parent.name}' and '${b.tpe.render}' on '${other.parent.name}', so the fields cannot merge",
          other.field.origin
        )
      case _ => ()
    }
    val mayOverlap =
      first.parent.name == other.parent.name || first.parent.kind != TypeKind.ObjectType || other.parent.kind != TypeKind.ObjectType
    if (mayOverlap) {
      val on =
        if (first.parent.name == other.parent.name) s"'${first.parent.name}'"
        else s"'${first.parent.name}' and '${other.parent.name}'"
      if (first.field.name != other.field.name) {
        err(
          ValidationRule.FieldSelectionMerging,
          s"response key '$key' is used for both '${first.field.name}' and '${other.field.name}' on $on",
          other.field.origin
        )
      } else if (argumentsOf(first.field) != argumentsOf(other.field)) {
        err(
          ValidationRule.FieldSelectionMerging,
          s"response key '$key' is selected twice with different arguments on $on",
          other.field.origin
        )
      } else {
        beneath(schema, first).foreach { child =>
          checkFieldsCanMerge(schema, document, child, first.field.selections ++ other.field.selections, err)
        }
      }
    }
  }

  /** `SameResponseShape`: the same wrapping, the same leaf, or composites whose merged selections have the same shape.
    */
  private def sameResponseShape(
      schema: Schema,
      document: ExecutableDocument,
      first: TypeRef,
      firstSelections: List[Selection],
      other: TypeRef,
      otherSelections: List[Selection]
  ): Boolean =
    (first, other) match {
      case (TypeRef.NonNull(a), TypeRef.NonNull(b))          =>
        sameResponseShape(schema, document, a, firstSelections, b, otherSelections)
      case (TypeRef.NonNull(_), _) | (_, TypeRef.NonNull(_)) => false
      case (TypeRef.ListOf(a), TypeRef.ListOf(b))            =>
        sameResponseShape(schema, document, a, firstSelections, b, otherSelections)
      case (TypeRef.ListOf(_), _) | (_, TypeRef.ListOf(_))   => false
      case (TypeRef.Named(a), TypeRef.Named(b))              =>
        if (isLeaf(schema, a) || isLeaf(schema, b)) a == b
        else
          (compositeType(schema, a), compositeType(schema, b)) match {
            case (Some(typeA), Some(typeB)) =>
              val merged =
                collectFields(schema, document, typeA, firstSelections, Set.empty) ++
                  collectFields(schema, document, typeB, otherSelections, Set.empty)
              merged.groupBy(_.field.responseKey).values.forall { occurrences =>
                occurrences.tails.forall {
                  case x :: rest =>
                    rest.forall { y =>
                      (x.definition, y.definition) match {
                        case (Some(dx), Some(dy)) =>
                          sameResponseShape(schema, document, dx.tpe, x.field.selections, dy.tpe, y.field.selections)
                        case _                    => true
                      }
                    }
                  case Nil       => true
                }
              }
            case _                          => true // an undefined type is reported where it is selected
          }
    }

  /** A field reached through fragments: the type it was selected on, and its definition when the type has it. */
  final private case class Collected(parent: TypeDef, field: FieldSelection, definition: Option[FieldDef])

  private def collectFields(
      schema: Schema,
      document: ExecutableDocument,
      parent: TypeDef,
      selections: List[Selection],
      seen: Set[String]
  ): List[Collected] =
    selections.flatMap {
      case field: FieldSelection  => List(Collected(parent, field, fieldDef(parent, field.name)))
      case inline: InlineFragment =>
        inline.typeCondition.fold(Option(parent))(compositeType(schema, _)).toList.flatMap { on =>
          collectFields(schema, document, on, inline.selections, seen)
        }
      case spread: FragmentSpread =>
        if (seen(spread.name)) Nil
        else
          document.fragment(spread.name).toList.flatMap { fragment =>
            compositeType(schema, fragment.typeCondition).toList.flatMap { on =>
              collectFields(schema, document, on, fragment.selections, seen + spread.name)
            }
          }
    }

  /** The composite type a collected field's selections are on, if it has one. */
  private def beneath(schema: Schema, collected: Collected): Option[TypeDef] =
    collected.definition.flatMap(definition => compositeType(schema, definition.tpe.named))

  private def argumentsOf(field: FieldSelection): List[(String, Value)]                                         =
    field.args.sortBy(_.name).map(a => a.name -> a.value)

  /** 5.6.1 to 5.6.4: a literal fits the type it is given for. A variable is checked where it is used, by 5.8.5. */
  private def checkValue(schema: Schema, tpe: TypeRef, value: Value, where: String, at: Origin, err: Err): Unit =
    (tpe, value) match {
      case (_, Value.VariableValue(_))                   => ()
      case (TypeRef.NonNull(_), Value.NullValue)         =>
        err(ValidationRule.ValuesOfCorrectType, s"$where is null, but its type '${tpe.render}' is non-null", at)
      case (_, Value.NullValue)                          => ()
      case (TypeRef.NonNull(of), _)                      => checkValue(schema, of, value, where, at, err)
      case (TypeRef.ListOf(of), Value.ListValue(values)) =>
        values.zipWithIndex.foreach { case (element, index) =>
          checkValue(schema, of, element, s"$where[$index]", at, err)
        }
      // a single value coerces to a list of one
      case (TypeRef.ListOf(of), single)                  => checkValue(schema, of, single, where, at, err)
      case (TypeRef.Named(name), _)                      => checkNamedValue(schema, name, value, where, at, err)
    }

  private def checkNamedValue(schema: Schema, name: String, value: Value, where: String, at: Origin, err: Err): Unit = {
    def wrong(expected: String): Unit =
      err(ValidationRule.ValuesOfCorrectType, s"$where is $expected, but got ${describe(value)}", at)

    (name, value) match {
      case ("Int", Value.IntValue(int))                       =>
        if (!int.isValidInt)
          err(ValidationRule.ValuesOfCorrectType, s"$where is an Int, but the value does not fit in 32 bits", at)
      case ("Int", _)                                         => wrong("an Int")
      case ("Float", Value.IntValue(_) | Value.FloatValue(_)) => ()
      case ("Float", _)                                       => wrong("a Float")
      case ("String", Value.StringValue(_))                   => ()
      case ("String", _)                                      => wrong("a String")
      case ("Boolean", Value.BooleanValue(_))                 => ()
      case ("Boolean", _)                                     => wrong("a Boolean")
      case ("ID", Value.StringValue(_) | Value.IntValue(_))   => ()
      case ("ID", _)                                          => wrong("an ID")
      case _                                                  =>
        schema.types.get(name) match {
          case Some(tpe) if tpe.kind == TypeKind.EnumType        =>
            value match {
              case Value.EnumValue(member) if tpe.enumValues.exists(_.name == member) => ()
              case Value.EnumValue(member)                                            =>
                err(
                  ValidationRule.ValuesOfCorrectType,
                  s"$where is the enum $name, but $member is not one of its values: ${tpe.enumValues.map(_.name).mkString(", ")}",
                  at
                )
              case Value.StringValue(text)                                            =>
                err(
                  ValidationRule.ValuesOfCorrectType,
                  s"$where is the enum $name, but got the string \"$text\": an enum value is written without quotes",
                  at
                )
              case _                                                                  => wrong(s"the enum $name")
            }
          case Some(tpe) if tpe.kind == TypeKind.InputObjectType =>
            value match {
              case Value.ObjectValue(fields) => checkInputObject(schema, tpe, fields, where, at, err)
              case _                         => wrong(s"the input object $name")
            }
          // a custom scalar takes any literal; a type that is not defined is reported where it is declared
          case _                                                 => ()
        }
    }
  }

  private def checkInputObject(
      schema: Schema,
      tpe: TypeDef,
      fields: List[(String, Value)],
      where: String,
      at: Origin,
      err: Err
  ): Unit = {
    fields.groupBy(_._1).foreach { case (field, occurrences) =>
      if (occurrences.size > 1) {
        err(ValidationRule.InputObjectFieldUniqueness, s"$where has field '$field' ${occurrences.size} times", at)
      }
    }
    fields.foreach { case (field, value) =>
      tpe.fields.find(_.name == field) match {
        case None             =>
          err(
            ValidationRule.InputObjectFieldNames,
            s"$where has no field '$field'",
            at
          )
        case Some(definition) => checkValue(schema, definition.tpe, value, s"$where.$field", at, err)
      }
    }
    tpe.fields.filter(field => field.tpe.isNonNull && field.defaultValue.isEmpty).foreach { required =>
      if (!fields.exists(_._1 == required.name)) {
        err(
          ValidationRule.InputObjectRequiredFields,
          s"$where requires field '${required.name}'",
          at
        )
      }
    }
  }

  private def describe(value: Value): String = value match {
    case Value.IntValue(int)         => s"the number $int"
    case Value.FloatValue(float)     => s"the number $float"
    case Value.StringValue(text)     => s"the string \"$text\""
    case Value.BooleanValue(boolean) => boolean.toString
    case Value.EnumValue(member)     => s"the enum value $member"
    case Value.NullValue             => "null"
    case Value.ListValue(_)          => "a list"
    case Value.ObjectValue(_)        => "an object"
    case Value.VariableValue(name)   => s"$$$name"
  }

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
        variable.default.foreach { default =>
          checkValue(
            schema,
            variable.tpe,
            default,
            s"the default of variable '$$${variable.name}'",
            variable.origin,
            err
          )
        }
      }
    }

  /** 5.8.3 and 5.8.5: every variable used is defined by the operation, and its type fits the position it is used in,
    * through every fragment the operation reaches.
    */
  private def checkVariableUsages(schema: Schema, document: ExecutableDocument, err: Err): Unit =
    document.operations.foreach { operation =>
      val defined = operation.variables.map(variable => variable.name -> variable).toMap
      val label   = operation.name.fold("the anonymous operation")(n => s"operation '$n'")
      val root    = rootTypeName(schema, operation.kind).flatMap(compositeType(schema, _))
      usages(schema, document, root, operation.selections, Set.empty).foreach { usage =>
        defined.get(usage.name) match {
          case None           =>
            err(
              ValidationRule.UndefinedVariables,
              s"variable '$$${usage.name}' is used but not defined by $label",
              usage.at
            )
          case Some(variable) =>
            usage.location.foreach { case (location, hasDefault) =>
              if (!usageAllowed(variable, location, hasDefault)) {
                err(
                  ValidationRule.VariableUsagesAllowed,
                  s"variable '$$${usage.name}' is a '${variable.tpe.render}', which cannot be used where a '${location.render}' is expected",
                  usage.at
                )
              }
            }
        }
      }
    }

  /** A variable's declared type against the type of a position, as the specification's `IsVariableUsageAllowed`: a
    * nullable variable may fill a non-null position only when it or the position has a default.
    */
  private def usageAllowed(variable: VariableDefinition, location: TypeRef, locationHasDefault: Boolean): Boolean =
    location match {
      case TypeRef.NonNull(of) if !variable.tpe.isNonNull =>
        val variableHasDefault = variable.default.exists(_ != Value.NullValue)
        (variableHasDefault || locationHasDefault) && compatible(variable.tpe, of)
      case _                                              => compatible(variable.tpe, location)
    }

  private def compatible(variable: TypeRef, location: TypeRef): Boolean =
    (variable, location) match {
      case (TypeRef.NonNull(v), TypeRef.NonNull(l)) => compatible(v, l)
      case (_, TypeRef.NonNull(_))                  => false
      case (TypeRef.NonNull(v), _)                  => compatible(v, location)
      case (TypeRef.ListOf(v), TypeRef.ListOf(l))   => compatible(v, l)
      case (TypeRef.ListOf(_), _)                   => false
      case (_, TypeRef.ListOf(_))                   => false
      case (TypeRef.Named(v), TypeRef.Named(l))     => v == l
    }

  /** One use of a variable: the type of the position, with whether it has a default, when the position is known. */
  final private case class Usage(name: String, location: Option[(TypeRef, Boolean)], at: Origin)

  private val conditionArgs = List(InputValueDef("if", TypeRef.NonNull(TypeRef.Named("Boolean"))))

  private def usages(
      schema: Schema,
      document: ExecutableDocument,
      parent: Option[TypeDef],
      selections: List[Selection],
      seen: Set[String]
  ): List[Usage] = {
    def inArgs(args: List[Argument], definitions: List[InputValueDef]): List[Usage] =
      args.flatMap { arg =>
        definitions.find(_.name == arg.name) match {
          case Some(definition) => inValue(schema, Some((definition.tpe, definition.hasDefault)), arg.value, arg.origin)
          case None             => inValue(schema, None, arg.value, arg.origin)
        }
      }
    def inDirectives(directives: List[Directive]): List[Usage]                      =
      directives.flatMap { directive =>
        inArgs(directive.args, if (directiveLocations.contains(directive.name)) conditionArgs else Nil)
      }

    selections.flatMap {
      case field: FieldSelection  =>
        val definition = parent.flatMap(fieldDef(_, field.name))
        val child      = definition.flatMap(d => compositeType(schema, d.tpe.named))
        inArgs(field.args, definition.map(_.args).getOrElse(Nil)) ++
          inDirectives(field.directives) ++
          usages(schema, document, child, field.selections, seen)
      case spread: FragmentSpread =>
        val inFragment =
          if (seen(spread.name)) Nil
          else
            document.fragment(spread.name).toList.flatMap { fragment =>
              usages(
                schema,
                document,
                compositeType(schema, fragment.typeCondition),
                fragment.selections,
                seen + spread.name
              )
            }
        inDirectives(spread.directives) ++ inFragment
      case inline: InlineFragment =>
        val on = inline.typeCondition.fold(parent)(compositeType(schema, _))
        inDirectives(inline.directives) ++ usages(schema, document, on, inline.selections, seen)
    }
  }

  /** The variables in a value, each with the type of its position when the value's own type is known. */
  private def inValue(
      schema: Schema,
      position: Option[(TypeRef, Boolean)],
      value: Value,
      at: Origin
  ): List[Usage] =
    value match {
      case Value.VariableValue(name) => List(Usage(name, position, at))
      case Value.ListValue(values)   =>
        val element = position.flatMap { case (tpe, _) => elementOf(tpe).map(_ -> false) }
        values.flatMap(inValue(schema, element, _, at))
      case Value.ObjectValue(fields) =>
        val input = position.map(_._1.named).flatMap(schema.types.get).filter(_.kind == TypeKind.InputObjectType)
        fields.flatMap { case (name, fieldValue) =>
          val field = input.flatMap(_.fields.find(_.name == name)).map(f => (f.tpe, f.defaultValue.isDefined))
          inValue(schema, field, fieldValue, at)
        }
      case _                         => Nil
    }

  private def elementOf(tpe: TypeRef): Option[TypeRef] = tpe match {
    case TypeRef.NonNull(of) => elementOf(of)
    case TypeRef.ListOf(of)  => Some(of)
    case TypeRef.Named(_)    => None
  }

  private def spreadsIn(selections: List[Selection]): List[String] =
    selections.flatMap {
      case field: FieldSelection  => spreadsIn(field.selections)
      case spread: FragmentSpread => List(spread.name)
      case inline: InlineFragment => spreadsIn(inline.selections)
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
