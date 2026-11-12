package net.andimiller.torpor.schema

import io.circe.Json
import net.andimiller.torpor.core.Value

/** Introspection, answered straight from the merged schema.
  *
  * The graph is lazy and driven by the client's selection set, because `__Type` is recursive: an eager encoder either
  * loops or invents a depth. `fields`, `ofType` and friends are thunks, resolved only when a selection asks.
  *
  * Introspection-only documents skip validation and planning entirely: no resolver runs and no plan is built. Anything
  * that is not purely `__schema`/`__type`/`__typename` at the root returns `None` and takes the normal path.
  *
  * `@deprecated` is reported on fields and enum values, and input defaults are rendered as GraphQL literals.
  * `includeDeprecated` is ignored: deprecated entries are always returned, with their flags.
  */
object Introspection {

  private val builtinScalars = Set("Int", "Float", "String", "Boolean", "ID")

  private sealed trait Node
  private final case class Leaf(json: Json)                                     extends Node
  private final case class Arr(items: List[Node])                               extends Node
  private final case class Obj(typeName: String, field: String => Option[Node]) extends Node

  def handle(schema: Schema, text: String, source: String = "query"): Option[Json] =
    SangriaBridge.parseExecutable(source, text).toOption.flatMap { document =>
      document.operations match {
        case operation :: Nil =>
          val fragments = document.fragments.map(fragment => fragment.name -> fragment).toMap
          val roots     = flatten(operation.selections, fragments)
          if (roots.nonEmpty && roots.forall(_.name.startsWith("__")))
            Some(
              Json.obj(
                roots.map { root =>
                  val key = root.alias.getOrElse(root.name)
                  key -> (root.name match {
                    case "__schema" =>
                      project(schemaNode(schema), root.selections, fragments)
                    case "__type"   =>
                      typeArgument(root)
                        .map(name => project(typeNode(schema, name), root.selections, fragments))
                        .getOrElse(Json.Null)
                    case _          => Json.fromString(rootTypeName(schema, operation))
                  })
                }: _*
              )
            )
          else None
        case _                => None
      }
    }

  // ---- projection -----------------------------------------------------------------------------

  private def project(node: Node, selections: List[Selection], fragments: Map[String, FragmentDefinition]): Json =
    node match {
      case Leaf(json)           => json
      case Arr(items)           => Json.fromValues(items.map(item => project(item, selections, fragments)))
      case Obj(typeName, field) =>
        Json.obj(
          flatten(selections, fragments).map { selection =>
            val key = selection.alias.getOrElse(selection.name)
            if (selection.name == "__typename") key -> Json.fromString(typeName)
            else
              key                                   -> field(selection.name)
                .map(next => project(next, selection.selections, fragments))
                .getOrElse(Json.Null)
          }: _*
        )
    }

  /** Fragments expanded, inline-fragment conditions ignored: there is one node type here (`__Type` and friends). */
  private def flatten(selections: List[Selection], fragments: Map[String, FragmentDefinition]): List[FieldSelection] = {
    def go(current: List[Selection], seen: Set[String]): List[FieldSelection] =
      current.flatMap {
        case field: FieldSelection  => List(field)
        case spread: FragmentSpread =>
          fragments
            .get(spread.name)
            .filterNot(fragment => seen(fragment.name))
            .toList
            .flatMap(fragment => go(fragment.selections, seen + fragment.name))
        case inline: InlineFragment => go(inline.selections, seen)
      }
    go(selections, Set.empty)
  }

  private def typeArgument(field: FieldSelection): Option[String]                  =
    field.args.collectFirst { case Argument("name", Value.StringValue(name), _) => name }

  private def rootTypeName(schema: Schema, operation: OperationDefinition): String =
    (operation.kind match {
      case OperationKind.Query        => schema.query
      case OperationKind.Mutation     => schema.mutation
      case OperationKind.Subscription => schema.subscription
    }).getOrElse(operation.kind.label.capitalize)

  // ---- the schema graph -----------------------------------------------------------------------

  private def schemaNode(schema: Schema): Node = Obj(
    "__Schema",
    {
      case "description"      => Some(Leaf(Json.Null))
      case "queryType"        => schema.query.map(name => refNode(schema, name))
      case "mutationType"     => schema.mutation.map(name => refNode(schema, name))
      case "subscriptionType" => schema.subscription.map(name => refNode(schema, name))
      case "types"            =>
        Some(
          Arr((builtinScalars.toList ++ schema.types.keys.toList).distinct.sorted.map(name => typeNode(schema, name)))
        )
      case "directives"       => Some(Arr(directiveNodes(schema)))
      case _                  => None
    }
  )

  /** The executable directives the validator knows, so tools can render their locations and arguments. */
  private def directiveNodes(schema: Schema): List[Node] =
    List(
      directiveNode(
        schema,
        "skip",
        "Skips the selection when `if` is true.",
        List("FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"),
        List(InputValueDef("if", TypeRef.NonNull(TypeRef.Named("Boolean"))))
      ),
      directiveNode(
        schema,
        "include",
        "Includes the selection when `if` is true.",
        List("FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"),
        List(InputValueDef("if", TypeRef.NonNull(TypeRef.Named("Boolean"))))
      ),
      directiveNode(
        schema,
        "deprecated",
        "Marks a field or enum value as deprecated.",
        List("FIELD_DEFINITION", "ENUM_VALUE"),
        Nil
      ),
      directiveNode(
        schema,
        "specifiedBy",
        "Points at the specification of a custom scalar.",
        List("SCALAR"),
        Nil
      )
    )

  private def directiveNode(
      schema: Schema,
      name: String,
      description: String,
      locations: List[String],
      args: List[InputValueDef]
  ): Node = Obj(
    "__Directive",
    {
      case "name"         => Some(Leaf(Json.fromString(name)))
      case "description"  => Some(Leaf(Json.fromString(description)))
      case "locations"    => Some(Arr(locations.map(location => Leaf(Json.fromString(location)))))
      case "args"         => Some(Arr(args.map(arg => inputValueNode(schema, arg))))
      case "isRepeatable" => Some(Leaf(Json.fromBoolean(false)))
      case _              => None
    }
  )

  /** A bare reference: enough for `fragment TypeRef on __Type { kind name ofType { … } }`. */
  private def refNode(schema: Schema, name: String): Node = Obj(
    "__Type",
    {
      case "kind"           => Some(Leaf(Json.fromString(kindOf(schema, name))))
      case "name"           => Some(Leaf(Json.fromString(name)))
      case "description"    => Some(Leaf(descriptionOf(schema, name)))
      case "ofType"         => Some(Leaf(Json.Null))
      case "specifiedByURL" =>
        Some(Leaf(schema.types.get(name).flatMap(_.specifiedByUrl).fold(Json.Null)(Json.fromString)))
      case _                => None
    }
  )

  private def wrapperNode(kind: String, of: Node): Node = Obj(
    "__Type",
    {
      case "kind"           => Some(Leaf(Json.fromString(kind)))
      case "name"           => Some(Leaf(Json.Null))
      case "ofType"         => Some(of)
      case "description"    => Some(Leaf(Json.Null))
      case "specifiedByURL" => Some(Leaf(Json.Null))
      case _                => None
    }
  )

  private def typeRefNode(schema: Schema, tpe: TypeRef): Node =
    tpe match {
      case TypeRef.Named(name) => refNode(schema, name)
      case TypeRef.ListOf(of)  => wrapperNode("LIST", typeRefNode(schema, of))
      case TypeRef.NonNull(of) => wrapperNode("NON_NULL", typeRefNode(schema, of))
    }

  private def typeNode(schema: Schema, name: String): Node = Obj(
    "__Type",
    {
      case "kind"           => Some(Leaf(Json.fromString(kindOf(schema, name))))
      case "name"           => Some(Leaf(Json.fromString(name)))
      case "description"    => Some(Leaf(descriptionOf(schema, name)))
      case "fields"         =>
        Some(
          schema.types
            .get(name)
            .filter(tpe => composite(tpe.kind))
            .fold(Leaf(Json.Null): Node)(tpe => Arr(tpe.fields.map(field => fieldNode(schema, field))))
        )
      case "interfaces"     =>
        Some(
          schema.types
            .get(name)
            .filter(tpe => tpe.kind == TypeKind.ObjectType || tpe.kind == TypeKind.InterfaceType)
            .fold(Leaf(Json.Null): Node)(tpe => Arr(tpe.interfaces.map(iface => refNode(schema, iface))))
        )
      case "possibleTypes"  => Some(possibleTypesNode(schema, name))
      case "enumValues"     =>
        Some(
          schema.types
            .get(name)
            .filter(_.kind == TypeKind.EnumType)
            .fold(Leaf(Json.Null): Node)(tpe => Arr(tpe.enumValues.map(enumValueNode)))
        )
      case "inputFields"    =>
        Some(
          schema.types
            .get(name)
            .filter(_.kind == TypeKind.InputObjectType)
            .fold(Leaf(Json.Null): Node)(tpe =>
              Arr(
                tpe.fields.map(field =>
                  inputValueNode(
                    schema,
                    InputValueDef(field.name, field.tpe, field.defaultValue, field.description)
                  )
                )
              )
            )
        )
      case "ofType"         => Some(Leaf(Json.Null))
      case "specifiedByURL" =>
        Some(Leaf(schema.types.get(name).flatMap(_.specifiedByUrl).fold(Json.Null)(Json.fromString)))
      case _                => None
    }
  )

  private def possibleTypesNode(schema: Schema, name: String): Node =
    schema.types.get(name) match {
      case Some(tpe) if tpe.kind == TypeKind.UnionType     =>
        Arr(tpe.unionMembers.map(member => refNode(schema, member)))
      case Some(tpe) if tpe.kind == TypeKind.InterfaceType =>
        Arr(
          schema.types.values.toList
            .filter(tpe2 => tpe2.kind == TypeKind.ObjectType && tpe2.interfaces.contains(name))
            .sortBy(_.name)
            .map(tpe2 => refNode(schema, tpe2.name))
        )
      case _                                               => Leaf(Json.Null)
    }

  private def fieldNode(schema: Schema, field: FieldDef): Node = Obj(
    "__Field",
    {
      case "name"              => Some(Leaf(Json.fromString(field.name)))
      case "description"       => Some(Leaf(field.description.fold(Json.Null)(Json.fromString)))
      case "args"              => Some(Arr(field.args.map(arg => inputValueNode(schema, arg))))
      case "type"              => Some(typeRefNode(schema, field.tpe))
      case "isDeprecated"      => Some(Leaf(Json.fromBoolean(field.deprecationReason.isDefined)))
      case "deprecationReason" => Some(Leaf(field.deprecationReason.fold(Json.Null)(Json.fromString)))
      case _                   => None
    }
  )

  private def inputValueNode(schema: Schema, value: InputValueDef): Node = Obj(
    "__InputValue",
    {
      case "name"         => Some(Leaf(Json.fromString(value.name)))
      case "description"  => Some(Leaf(value.description.fold(Json.Null)(Json.fromString)))
      case "type"         => Some(typeRefNode(schema, value.tpe))
      case "defaultValue" => Some(Leaf(value.default.fold(Json.Null)(v => Json.fromString(renderValue(v)))))
      case _              => None
    }
  )

  private def enumValueNode(value: EnumValueDef): Node = Obj(
    "__EnumValue",
    {
      case "name"              => Some(Leaf(Json.fromString(value.name)))
      case "description"       => Some(Leaf(value.description.fold(Json.Null)(Json.fromString)))
      case "isDeprecated"      => Some(Leaf(Json.fromBoolean(value.deprecationReason.isDefined)))
      case "deprecationReason" => Some(Leaf(value.deprecationReason.fold(Json.Null)(Json.fromString)))
      case _                   => None
    }
  )

  /** A default as a GraphQL literal, which is what `__InputValue.defaultValue` is defined to be. */
  private def renderValue(value: Value): String =
    value match {
      case Value.IntValue(v)         => v.toString
      case Value.FloatValue(v)       => v.toString
      case Value.StringValue(v)      =>
        "\"" + v.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
      case Value.BooleanValue(v)     => v.toString
      case Value.EnumValue(v)        => v
      case Value.NullValue           => "null"
      case Value.ListValue(values)   =>
        values.map(renderValue).mkString("[", ", ", "]")
      case Value.ObjectValue(fields) =>
        fields.map { case (name, v) => s"$name: ${renderValue(v)}" }.mkString("{", ", ", "}")
      case Value.VariableValue(n)    => "$" + n
    }

  private def kindOf(schema: Schema, name: String): String =
    if (builtinScalars(name)) "SCALAR"
    else
      schema.types.get(name) match {
        case None      => "SCALAR"
        case Some(tpe) =>
          tpe.kind match {
            case TypeKind.ObjectType      => "OBJECT"
            case TypeKind.InterfaceType   => "INTERFACE"
            case TypeKind.UnionType       => "UNION"
            case TypeKind.EnumType        => "ENUM"
            case TypeKind.InputObjectType => "INPUT_OBJECT"
            case TypeKind.ScalarType      => "SCALAR"
          }
      }

  private def descriptionOf(schema: Schema, name: String): Json =
    schema.types.get(name).flatMap(_.description).fold(Json.Null)(Json.fromString)

  private def composite(kind: TypeKind): Boolean =
    kind match {
      case TypeKind.ObjectType | TypeKind.InterfaceType | TypeKind.UnionType => true
      case _                                                                 => false
    }
}
