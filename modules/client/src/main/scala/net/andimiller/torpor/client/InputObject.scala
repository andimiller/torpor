package net.andimiller.torpor.client

import io.circe.{Encoder, Json, JsonObject}

import scala.language.dynamics
import scala.quoted.*

/** An input object written with `input(name = value, …)`. Its type lists each field it was given and the type of the
  * value, which is what a typed operation checks against the schema's input type when it is passed as a variable.
  */
final class InputObject[+Fields <: Tuple](val json: JsonObject)

object InputObject {

  given [I <: InputObject[Tuple]]: Encoder[I] = Encoder.instance(input => Json.fromJsonObject(input.json))

  /** Called by code the macro generates; not part of the API. A field given as `None` has no value, and is left out. */
  def of[Fields <: Tuple](fields: List[(String, Option[Json])]): InputObject[Fields] =
    new InputObject(JsonObject.fromIterable(fields.collect { case (name, Some(json)) => name -> json }))
}

/** Writes an input object from named fields, each encoded with the `Encoder` in scope for its type:
  * {{{
  * input(species = "DOG", first = Some(10), nickname = null)
  * }}}
  * A field that is `None` is left out, and one that is `null` is sent as null, which is not the same thing when the
  * field has a default. Nothing is checked here. Passing the result to a typed operation checks its fields against the
  * input type the operation's variable declares.
  */
object input extends Dynamic {

  transparent inline def applyDynamicNamed(inline method: String)(inline fields: (String, Any)*): InputObject[?] =
    ${ TypedMacro.input('fields) }
}
