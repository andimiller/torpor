package net.andimiller.torpor.schema

import cats.data.NonEmptyChain
import net.andimiller.torpor.core.Value
import sangria.ast
import sangria.parser.QueryParser

import scala.util.{Failure, Success}

/** The only file in the project that imports `sangria.*`.
  *
  * Everything here is a straight translation into Torpor's own AST, so swapping the parser (or forking the two sangria
  * modules) lands in this one file. Enforced by a grep in CI.
  */
private[schema] object SangriaBridge {

  final case class Parsed(source: String, types: List[TypeDef], extensions: List[Extension], roots: List[Root])

  /** A type extension, carrying only the parts it adds. */
  final case class Extension(tpe: TypeDef, origin: Origin)

  enum RootOperation(val label: String) {
    case Query extends RootOperation("query")
    case Mutation extends RootOperation("mutation")
    case Subscription extends RootOperation("subscription")
  }

  final case class Root(op: RootOperation, tpe: String, origin: Origin)

  def parse(fragment: SdlFragment): Either[NonEmptyChain[SchemaError], Parsed] =
    QueryParser.parse(fragment.sdl) match {
      case Failure(e)        =>
        val detail = Option(e.getMessage).getOrElse(e.toString)
        Left(NonEmptyChain.one(SchemaError.Parse(Origin(fragment.source), detail)))
      case Success(document) =>
        translate(fragment.source, document)
    }

  private def translate(source: String, document: ast.Document): Either[NonEmptyChain[SchemaError], Parsed] = {
    val types      = List.newBuilder[TypeDef]
    val extensions = List.newBuilder[Extension]
    val roots      = List.newBuilder[Root]
    val errors     = List.newBuilder[SchemaError]

    def at(location: Option[ast.AstLocation]): Origin = origin(source, location)

    document.definitions.foreach {
      case d: ast.ObjectTypeDefinition      =>
        types += TypeDef(
          d.name,
          TypeKind.ObjectType,
          d.description.map(_.value),
          d.fields.toList.map(fieldDef(source, _)),
          d.interfaces.toList.map(_.name),
          Nil,
          Nil,
          at(d.location)
        )
      case d: ast.InterfaceTypeDefinition   =>
        types += TypeDef(
          d.name,
          TypeKind.InterfaceType,
          d.description.map(_.value),
          d.fields.toList.map(fieldDef(source, _)),
          d.interfaces.toList.map(_.name),
          Nil,
          Nil,
          at(d.location)
        )
      case d: ast.UnionTypeDefinition       =>
        types += TypeDef
          .empty(d.name, TypeKind.UnionType, at(d.location))
          .copy(description = d.description.map(_.value), unionMembers = d.types.toList.map(_.name))
      case d: ast.EnumTypeDefinition        =>
        types += TypeDef
          .empty(d.name, TypeKind.EnumType, at(d.location))
          .copy(description = d.description.map(_.value), enumValues = d.values.toList.map(enumValueDef))
      case d: ast.InputObjectTypeDefinition =>
        types += TypeDef
          .empty(d.name, TypeKind.InputObjectType, at(d.location))
          .copy(
            description = d.description.map(_.value),
            fields = d.fields.toList.map(f =>
              FieldDef(
                f.name,
                typeRef(f.valueType),
                Nil,
                f.description.map(_.value),
                at(f.location),
                defaultValue = f.defaultValue.map(toValue)
              )
            )
          )
      case d: ast.ScalarTypeDefinition      =>
        types += TypeDef
          .empty(d.name, TypeKind.ScalarType, at(d.location))
          .copy(description = d.description.map(_.value), specifiedByUrl = specifiedBy(d.directives))

      case e: ast.ObjectTypeExtensionDefinition      =>
        extensions += Extension(
          TypeDef
            .empty(e.name, TypeKind.ObjectType, at(e.location))
            .copy(
              fields = e.fields.toList.map(fieldDef(source, _)),
              interfaces = e.interfaces.toList.map(_.name)
            ),
          at(e.location)
        )
      case e: ast.InterfaceTypeExtensionDefinition   =>
        extensions += Extension(
          TypeDef
            .empty(e.name, TypeKind.InterfaceType, at(e.location))
            .copy(
              fields = e.fields.toList.map(fieldDef(source, _)),
              interfaces = e.interfaces.toList.map(_.name)
            ),
          at(e.location)
        )
      case e: ast.UnionTypeExtensionDefinition       =>
        extensions += Extension(
          TypeDef.empty(e.name, TypeKind.UnionType, at(e.location)).copy(unionMembers = e.types.toList.map(_.name)),
          at(e.location)
        )
      case e: ast.EnumTypeExtensionDefinition        =>
        extensions += Extension(
          TypeDef
            .empty(e.name, TypeKind.EnumType, at(e.location))
            .copy(enumValues = e.values.toList.map(enumValueDef)),
          at(e.location)
        )
      case e: ast.InputObjectTypeExtensionDefinition =>
        extensions += Extension(
          TypeDef
            .empty(e.name, TypeKind.InputObjectType, at(e.location))
            .copy(
              fields = e.fields.toList
                .map(f =>
                  FieldDef(
                    f.name,
                    typeRef(f.valueType),
                    Nil,
                    f.description.map(_.value),
                    at(f.location),
                    defaultValue = f.defaultValue.map(toValue)
                  )
                )
            ),
          at(e.location)
        )
      case _: ast.ScalarTypeExtensionDefinition      =>
        () // scalars carry no members, nothing to merge

      case d: ast.SchemaDefinition          =>
        roots ++= d.operationTypes.toList.map(root(source, _))
      case e: ast.SchemaExtensionDefinition =>
        roots ++= e.operationTypes.toList.map(root(source, _))

      case d: ast.DirectiveDefinition =>
        errors += SchemaError.DirectiveDefinitionUnsupported(at(d.location), d.name)
      case d: ast.OperationDefinition =>
        val kind = d.operationType match {
          case ast.OperationType.Query        => "query"
          case ast.OperationType.Mutation     => "mutation"
          case ast.OperationType.Subscription => "subscription"
        }
        errors += SchemaError.ExecutableDefinitionFound(at(d.location), s"$kind operation")
      case f: ast.FragmentDefinition  =>
        errors += SchemaError.ExecutableDefinitionFound(at(f.location), "fragment")
    }

    NonEmptyChain.fromSeq(errors.result()) match {
      case Some(nec) => Left(nec)
      case None      => Right(Parsed(source, types.result(), extensions.result(), roots.result()))
    }
  }

  private def root(source: String, d: ast.OperationTypeDefinition): Root = {
    val op = d.operation match {
      case ast.OperationType.Query        => RootOperation.Query
      case ast.OperationType.Mutation     => RootOperation.Mutation
      case ast.OperationType.Subscription => RootOperation.Subscription
    }
    Root(op, d.tpe.name, origin(source, d.location))
  }

  private def fieldDef(source: String, f: ast.FieldDefinition): FieldDef =
    FieldDef(
      f.name,
      typeRef(f.fieldType),
      f.arguments.toList.map(iv =>
        InputValueDef(iv.name, typeRef(iv.valueType), iv.defaultValue.map(toValue), iv.description.map(_.value))
      ),
      f.description.map(_.value),
      origin(source, f.location),
      deprecationReason = deprecation(f.directives)
    )

  private def enumValueDef(v: ast.EnumValueDefinition): EnumValueDef =
    EnumValueDef(v.name, v.description.map(_.value), deprecation(v.directives))

  private def deprecation(directives: Seq[ast.Directive]): Option[String] =
    directives.find(_.name == "deprecated").map { directive =>
      directive.arguments
        .find(_.name == "reason")
        .flatMap(argument => stringValue(argument.value))
        .getOrElse("No longer supported")
    }

  private def specifiedBy(directives: Seq[ast.Directive]): Option[String] =
    directives
      .find(_.name == "specifiedBy")
      .flatMap(_.arguments.find(_.name == "url"))
      .flatMap(argument => stringValue(argument.value))

  private def stringValue(value: ast.Value): Option[String] =
    value match {
      case v: ast.StringValue => Some(v.value)
      case _                  => None
    }

  private def typeRef(t: ast.Type): TypeRef =
    t match {
      case ast.NamedType(name, _)     => TypeRef.Named(name)
      case ast.ListType(ofType, _)    => TypeRef.ListOf(typeRef(ofType))
      case ast.NotNullType(ofType, _) => TypeRef.NonNull(typeRef(ofType))
    }

  private def origin(source: String, location: Option[ast.AstLocation]): Origin =
    location match {
      case Some(l) => Origin(source, Some(l.line), Some(l.column))
      case None    => Origin(source)
    }

  // ---- executable documents ------------------------------------------------------------------

  def parseExecutable(
      source: String,
      text: String
  ): Either[NonEmptyChain[ValidationError], ExecutableDocument] =
    QueryParser.parse(text) match {
      case Failure(e)        =>
        val detail = Option(e.getMessage).getOrElse(e.toString)
        Left(NonEmptyChain.one(ValidationError(ValidationRule.DocumentSyntax, detail, Origin(source))))
      case Success(document) => translateExecutable(source, document)
    }

  private def translateExecutable(
      source: String,
      document: ast.Document
  ): Either[NonEmptyChain[ValidationError], ExecutableDocument] = {
    val operations = List.newBuilder[OperationDefinition]
    val fragments  = List.newBuilder[FragmentDefinition]
    val errors     = List.newBuilder[ValidationError]

    document.definitions.foreach {
      case d: ast.OperationDefinition =>
        operations += OperationDefinition(
          operationKind(d.operationType),
          d.name,
          d.variables.toList.map(v =>
            VariableDefinition(v.name, typeRef(v.tpe), v.defaultValue.map(toValue), origin(source, v.location))
          ),
          d.directives.toList.map(directive(source, _)),
          d.selections.toList.map(selection(source, _)),
          origin(source, d.location)
        )
      case f: ast.FragmentDefinition  =>
        fragments += FragmentDefinition(
          f.name,
          f.typeCondition.name,
          f.directives.toList.map(directive(source, _)),
          f.selections.toList.map(selection(source, _)),
          origin(source, f.location)
        )
      case other                      =>
        errors += ValidationError(
          ValidationRule.ExecutableDefinitions,
          s"a query document may only contain operations and fragments, but found: ${definitionKind(other)}",
          origin(source, other.location)
        )
    }

    NonEmptyChain.fromSeq(errors.result()) match {
      case Some(nec) => Left(nec)
      case None      => Right(ExecutableDocument(operations.result(), fragments.result()))
    }
  }

  private def operationKind(tpe: ast.OperationType): OperationKind =
    tpe match {
      case ast.OperationType.Query        => OperationKind.Query
      case ast.OperationType.Mutation     => OperationKind.Mutation
      case ast.OperationType.Subscription => OperationKind.Subscription
    }

  private def definitionKind(definition: ast.Definition): String =
    definition.getClass.getSimpleName
      .stripSuffix("Definition")
      .replaceAll("([a-z])([A-Z])", "$1 $2")
      .toLowerCase

  private def selection(source: String, node: ast.Selection): Selection =
    node match {
      case f: ast.Field          =>
        FieldSelection(
          f.alias,
          f.name,
          f.arguments.toList.map(argument(source, _)),
          f.directives.toList.map(directive(source, _)),
          f.selections.toList.map(selection(source, _)),
          origin(source, f.location)
        )
      case s: ast.FragmentSpread =>
        FragmentSpread(s.name, s.directives.toList.map(directive(source, _)), origin(source, s.location))
      case i: ast.InlineFragment =>
        InlineFragment(
          i.typeCondition.map(_.name),
          i.directives.toList.map(directive(source, _)),
          i.selections.toList.map(selection(source, _)),
          origin(source, i.location)
        )
    }

  private def argument(source: String, argument: ast.Argument): Argument =
    Argument(argument.name, toValue(argument.value), origin(source, argument.location))

  private def directive(source: String, directive: ast.Directive): Directive =
    Directive(directive.name, directive.arguments.toList.map(argument(source, _)), origin(source, directive.location))

  private def toValue(node: ast.Value): Value =
    node match {
      case v: ast.IntValue        => Value.IntValue(BigInt(v.value))
      case v: ast.BigIntValue     => Value.IntValue(v.value)
      case v: ast.FloatValue      => Value.FloatValue(BigDecimal(v.value))
      case v: ast.BigDecimalValue => Value.FloatValue(v.value)
      case v: ast.StringValue     => Value.StringValue(v.value)
      case v: ast.BooleanValue    => Value.BooleanValue(v.value)
      case v: ast.EnumValue       => Value.EnumValue(v.value)
      case _: ast.NullValue       => Value.NullValue
      case v: ast.ListValue       => Value.ListValue(v.values.toList.map(toValue))
      case v: ast.ObjectValue     => Value.ObjectValue(v.fields.toList.map(f => f.name -> toValue(f.value)))
      case v: ast.VariableValue   => Value.VariableValue(v.name)
    }
}
