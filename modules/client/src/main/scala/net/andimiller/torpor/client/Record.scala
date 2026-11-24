package net.andimiller.torpor.client

import io.circe.Decoder

/** An object in a response, as a typed operation reads it.
  *
  * The macro that types an operation refines this with one member for each field the operation selects, so
  * `data.pets.head.name` compiles only if the operation selects `name`, and has the type the schema gives it. Values
  * are decoded once, when the response arrives, and members read them back by name.
  */
final class Record(private val values: Map[String, Any]) extends Selectable {
  def selectDynamic(name: String): Any = values(name)

  override def equals(other: Any): Boolean = other match {
    case that: Record => values == that.values
    case _            => false
  }

  override def hashCode: Int = values.hashCode

  override def toString: String =
    values.toList.sortBy(_._1).map { case (name, value) => s"$name = $value" }.mkString("Record(", ", ", ")")
}

object Record {

  /** Called by code the macro generates; not part of the API. A field the response leaves out is read with a failed
    * cursor, which is what an `Option` decodes as `None`.
    */
  def decoder(fields: List[(String, Decoder[?])]): Decoder[Record] =
    Decoder.instance { cursor =>
      fields
        .foldLeft[Decoder.Result[Map[String, Any]]](Right(Map.empty)) { case (decoded, (name, decoder)) =>
          decoded.flatMap(values => decoder.tryDecode(cursor.downField(name)).map(value => values + (name -> value)))
        }
        .map(new Record(_))
    }
}
