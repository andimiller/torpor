package net.andimiller.torpor.client

/** The shape of a Scala type, as far as a GraphQL schema can say anything about it.
  *
  * The macro reads a shape off each value given for a variable, and [[Check]] compares it with the input's type.
  * Keeping the comparison free of compiler types is what lets it be tested as ordinary code.
  */
enum Shape {

  /** `Option[A]`. */
  case Optional(of: Shape)

  /** A collection of `A`. */
  case Many(of: Shape)

  /** A case class, by its fields in order. */
  case Record(name: String, fields: List[(String, Shape)])

  /** A sealed trait, by its case classes. No input type is one. */
  case Sealed(name: String, members: List[Shape])

  /** Anything else: a primitive, or a type whose own codec decides what it writes, such as an id. */
  case Leaf(name: String)

  /** A type the document can say nothing about, so it fits any leaf: an opaque type, whose own codec decides. */
  case Trusted(name: String)

  /** One of these string literals: a literal such as `"DOG"`, or a union of them such as `"DOG" | "CAT"`, which is what
    * an enum is written as.
    */
  case Literals(values: List[String])

  def render: String = this match {
    case Literals(values) => values.map(value => s"\"$value\"").mkString(" | ")
    case Optional(of)     => s"Option[${of.render}]"
    case Many(of)         => s"List[${of.render}]"
    case Record(name, _)  => name
    case Sealed(name, _)  => name
    case Leaf(name)       => name
    case Trusted(name)    => name
  }
}
