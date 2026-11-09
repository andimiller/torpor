package net.andimiller.torpor.schema

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

enum TypeRef {
  case Named(name: String)
  case ListOf(of: TypeRef)
  case NonNull(of: TypeRef)

  /** The type name at the bottom of any list/non-null wrappers. */
  def named: String = this match {
    case Named(name) => name
    case ListOf(of)  => of.named
    case NonNull(of) => of.named
  }

  def isNonNull: Boolean = this match {
    case NonNull(_) => true
    case _          => false
  }
}

final case class InputValueDef(
    name: String,
    tpe: TypeRef,
    hasDefault: Boolean = false,
    description: Option[String] = None
)

final case class FieldDef(
    name: String,
    tpe: TypeRef,
    args: List[InputValueDef],
    description: Option[String],
    origin: Origin,
    /** True when the field arrived through `extend type`, which means a resolver serves it and the planner must give it
      * its own node. Fields declared in the type definition are returned by whatever produced the value.
      */
    fromExtension: Boolean = false
)

final case class TypeDef(
    name: String,
    kind: TypeKind,
    description: Option[String],
    fields: List[FieldDef],
    interfaces: List[String],
    unionMembers: List[String],
    enumValues: List[String],
    origin: Origin
)

object TypeDef {
  def empty(name: String, kind: TypeKind, origin: Origin): TypeDef =
    TypeDef(name, kind, None, Nil, Nil, Nil, Nil, origin)
}
