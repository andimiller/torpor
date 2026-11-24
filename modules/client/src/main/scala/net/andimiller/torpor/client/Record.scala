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
    * cursor, which is what an `Option` decodes as `None`. A view is read from the object itself, as one of the types it
    * can be: see [[member]].
    */
  def decoder(fields: List[(String, Decoder[?])], views: List[(String, Decoder[?])] = Nil): Decoder[Record] =
    Decoder.instance { cursor =>
      val read = fields.map { case (name, decoder) => (name, decoder, cursor.downField(name)) } ++
        views.map { case (name, decoder) => (name, decoder, cursor) }
      read
        .foldLeft[Decoder.Result[Map[String, Any]]](Right(Map.empty)) { case (decoded, (name, decoder, at)) =>
          decoded.flatMap(values => decoder.tryDecode(at).map(value => values + (name -> value)))
        }
        .map(new Record(_))
    }

  /** Called by code the macro generates; not part of the API. The value as one type it can be, when the `__typename`
    * under `key` says it is that type, read with that type's decoder.
    */
  def member(key: String, name: String, decoder: Decoder[?]): Decoder[Option[Any]] =
    Decoder.instance { cursor =>
      cursor.get[String](key).flatMap { typename =>
        if (typename == name) decoder.tryDecode(cursor).map(Some(_)) else Right(None)
      }
    }
}
