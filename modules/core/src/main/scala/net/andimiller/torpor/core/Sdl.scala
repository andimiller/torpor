package net.andimiller.torpor.core

/** SDL produced by the `schema"""…"""` macro: the text to merge, with every field coordinate it declares in its type.
  *
  * The macro refines the value with one member per type, each with one member per field, so `sdl.Query.character`
  * compiles only if the SDL declares `Query.character`, and an editor can complete it. The member's type names the
  * coordinate's parent type and field, and carries the field's arguments, which is what types a root resolver's `args`.
  *
  * `Required` is a tuple of `"Type.field"` literals: the fields a source serving this SDL must resolve, which are the
  * ones it adds with `extend type` and those of the root types it declares.
  */
final class Sdl[Required <: Tuple](val text: String) extends Selectable {

  /** `sdl.Query`. The macro's refinement decides which names typecheck; this only builds the value. */
  def selectDynamic(typeName: String): Sdl.TypeFields = Sdl.TypeFields(typeName)
}

object Sdl {

  final class TypeFields(typeName: String) extends Selectable {
    def selectDynamic(fieldName: String): FieldCoordinate = FieldCoordinate(typeName, fieldName)
  }
}
