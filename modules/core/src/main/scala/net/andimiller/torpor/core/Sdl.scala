package net.andimiller.torpor.core

/** SDL produced by the `schema"""…"""` macro: the text to merge, plus every field coordinate it declares.
  *
  * The map is built and validated at compile time, so it cannot be incomplete; `coordinate` is a lookup, and a miss is
  * a programming error with a readable message rather than a startup surprise.
  */
final case class Sdl(text: String, coordinates: Map[(String, String), FieldCoordinate]) {
  def coordinate(typeName: String, fieldName: String): FieldCoordinate =
    coordinates.getOrElse(
      (typeName, fieldName),
      throw new NoSuchElementException(
        s"the SDL declares no field '$typeName.$fieldName'; it has: ${coordinates.keys.toList
          .map { case (t, f) => s"$t.$f" }
          .sorted
          .mkString(", ")}"
      )
    )
}
