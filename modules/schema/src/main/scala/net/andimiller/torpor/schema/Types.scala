package net.andimiller.torpor.schema

export net.andimiller.torpor.core.TypeRef

/** One SDL fragment and the id of the source that serves it. */
final case class SdlFragment(source: String, sdl: String)

enum TypeKind(val label: String) {
  case ObjectType extends TypeKind("object")
  case InterfaceType extends TypeKind("interface")
  case UnionType extends TypeKind("union")
  case EnumType extends TypeKind("enum")
  case InputObjectType extends TypeKind("input object")
  case ScalarType extends TypeKind("scalar")
}

final case class InputValueDef(
    name: String,
    tpe: TypeRef,
    /** The SDL default, kept as a literal so it can fill omitted arguments and be printed by introspection. */
    default: Option[net.andimiller.torpor.core.Value] = None,
    description: Option[String] = None
) {
  def hasDefault: Boolean = default.isDefined
}

final case class FieldDef(
    name: String,
    tpe: TypeRef,
    args: List[InputValueDef],
    description: Option[String],
    origin: Origin,
    /** True when the field arrived through `extend type`, which means a resolver serves it and the planner must give it
      * its own node. Fields declared in the type definition are returned by whatever produced the value.
      */
    fromExtension: Boolean = false,
    deprecationReason: Option[String] = None,
    /** The SDL default of an input-object field; unused for output fields. */
    defaultValue: Option[net.andimiller.torpor.core.Value] = None
)

final case class EnumValueDef(
    name: String,
    description: Option[String] = None,
    deprecationReason: Option[String] = None
)

final case class TypeDef(
    name: String,
    kind: TypeKind,
    description: Option[String],
    fields: List[FieldDef],
    interfaces: List[String],
    unionMembers: List[String],
    enumValues: List[EnumValueDef],
    origin: Origin,
    /** `@specifiedBy(url: …)` on a custom scalar. */
    specifiedByUrl: Option[String] = None
)

object TypeDef {
  def empty(name: String, kind: TypeKind, origin: Origin): TypeDef =
    TypeDef(name, kind, None, Nil, Nil, Nil, Nil, origin)
}
