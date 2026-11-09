package net.andimiller.torpor.schema

/** A failure to turn SDL fragments into one schema.
  *
  * `origin` is the location to blame when there is one: the second declaration of a duplicate, the extension that does
  * not fit, the root definition naming a missing type.
  */
sealed trait SchemaError {
  def origin: Origin

  /** Human-readable, single line. This is what startup errors and test failures print. */
  def render: String
}

object SchemaError {

  final case class Parse(origin: Origin, detail: String) extends SchemaError {
    def render: String = s"${origin.render}: cannot parse SDL: $detail"
  }

  final case class ExecutableDefinitionFound(origin: Origin, kind: String) extends SchemaError {
    def render: String =
      s"${origin.render}: expected a schema document but found a $kind definition; client queries belong at request time"
  }

  final case class DirectiveDefinitionUnsupported(origin: Origin, name: String) extends SchemaError {
    def render: String =
      s"${origin.render}: custom directive '@$name' is not supported in v1"
  }

  final case class DuplicateType(name: String, first: Origin, second: Origin) extends SchemaError {
    def origin: Origin = second
    def render: String =
      s"type '$name' is defined twice (${first.render} and ${second.render}); one source owns a type, others may extend it"
  }

  final case class FieldCollision(tpe: String, field: String, first: Origin, second: Origin) extends SchemaError {
    def origin: Origin = second
    def render: String = s"field '$tpe.$field' is declared twice (${first.render} and ${second.render})"
  }

  final case class UnknownExtension(tpe: String, origin: Origin) extends SchemaError {
    def render: String =
      s"${origin.render}: cannot extend type '$tpe' because no source defines it"
  }

  final case class KindMismatch(
      tpe: String,
      defined: Origin,
      definedKind: TypeKind,
      extendedKind: TypeKind,
      origin: Origin
  ) extends SchemaError {
    def render: String =
      s"${origin.render}: extends '$tpe' as a ${extendedKind.label}, but ${defined.render} defines it as a ${definedKind.label}"
  }

  final case class RootConflict(operation: String, firstType: String, first: Origin, secondType: String, second: Origin)
      extends SchemaError {
    def origin: Origin = second
    def render: String =
      s"root $operation is declared as '$firstType' (${first.render}) and as '$secondType' (${second.render})"
  }

  final case class RootNotDefined(operation: String, tpe: String, origin: Origin) extends SchemaError {
    def render: String =
      s"${origin.render}: root $operation type '$tpe' is not defined by any source"
  }
}
