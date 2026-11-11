package net.andimiller.torpor.core

/** Binds a GraphQL scalar to the Scala type its argument values decode to, for the arguments the schema macro types:
  * `given Scalar["ID", Long]()`.
  *
  * The macro looks the binding up where `schema"""…"""` is written, so declare it before the SDL. Without one, `ID`
  * arguments are `String` and custom scalars are circe `Json`. The binding is per scalar, not per field: a field whose
  * arguments need another decoding reads them with `.args[A]`.
  */
open class Scalar[Name <: String, A]
