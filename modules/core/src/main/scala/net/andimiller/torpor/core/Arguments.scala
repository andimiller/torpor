package net.andimiller.torpor.core

import io.circe.Decoder

import scala.compiletime.{constValue, erasedValue, summonInline}

/** A field's arguments as the schema macro types them. The type is written twice:
  *
  *   - `Spec` lists each argument's name and the type its decoder reads, such as `(("id", Option[Long]), ("species",
  *     Option[OneOf[("DOG", "CAT")]]))`. `decoder` recurses over it, so no macro is needed where a resolver is written.
  *   - a refinement adds one member per argument, such as `{ val id: Option[Long]; val species: Option["DOG" | "CAT"]
  *     }`, so that `args.id` compiles.
  *
  * Values are decoded once, before the resolver runs, and members read them back by name.
  */
final class Arguments[Spec <: Tuple](private val values: Map[String, Any]) extends Selectable {
  def selectDynamic(name: String): Any = values(name)

  override def equals(other: Any): Boolean = other match {
    case that: Arguments[?] => values == that.values
    case _                  => false
  }

  override def hashCode: Int = values.hashCode

  override def toString: String =
    values.toList.sortBy(_._1).map { case (name, value) => s"$name = $value" }.mkString("Arguments(", ", ", ")")
}

object Arguments {

  /** Marks an enum argument in `Spec`: the decoder accepts a string in `Values`. The value is that `String`, which args
    * read as a literal union such as `"DOG" | "CAT"`.
    */
  sealed trait OneOf[Values <: Tuple]

  /** The `Spec` of an `Arguments` type, refined or not. */
  type SpecOf[A] = A match {
    case Arguments[spec] => spec
  }

  /** A decoder for an `Arguments` type, built from its `Spec`: the `Decoder` in scope for each argument's type. */
  inline def decoder[A]: Decoder[A] = record(fields[SpecOf[A]]).asInstanceOf[Decoder[A]]

  /** Nested `Arguments`, for input objects. */
  inline given decodeArguments[A <: Arguments[?]]: Decoder[A] = decoder[A]

  inline given decodeOneOf[Values <: Tuple]: Decoder[OneOf[Values]] =
    oneOf(strings[Values].toSet).asInstanceOf[Decoder[OneOf[Values]]]

  private inline def strings[Values <: Tuple]: List[String] =
    inline erasedValue[Values] match {
      case _: EmptyTuple      => Nil
      case _: (value *: rest) => constValue[value].toString :: strings[rest]
    }

  private inline def fields[Spec]: List[(String, Decoder[?])] =
    inline erasedValue[Spec] match {
      case _: EmptyTuple              => Nil
      case _: ((name, value) *: rest) => (constValue[name].toString, summonInline[Decoder[value]]) :: fields[rest]
    }

  /** Called by code `decoder` generates; not part of the API. */
  def record(fields: List[(String, Decoder[?])]): Decoder[Arguments[?]] =
    Decoder.instance { cursor =>
      fields
        .foldLeft[Decoder.Result[Map[String, Any]]](Right(Map.empty)) { case (decoded, (name, decoder)) =>
          decoded.flatMap(values => decoder.tryDecode(cursor.downField(name)).map(value => values + (name -> value)))
        }
        .map(new Arguments(_))
    }

  /** Called by code `decodeOneOf` generates; not part of the API. */
  def oneOf(values: Set[String]): Decoder[String] =
    Decoder.decodeString.emap(value =>
      Either.cond(values(value), value, s"'$value' is not one of ${values.toList.sorted.mkString(", ")}")
    )
}
