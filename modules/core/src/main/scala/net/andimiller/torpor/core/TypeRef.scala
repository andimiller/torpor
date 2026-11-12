package net.andimiller.torpor.core

/** A GraphQL type reference. Lives in `core` because the plan carries field types: response assembly needs to know
  * whether a field is non-null to bubble nulls to the nearest nullable ancestor.
  */
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
