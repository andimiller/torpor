package net.andimiller.torpor.client

import io.circe.{Decoder, JsonObject}

import scala.language.dynamics
import scala.quoted.*

/** An operation ready to send: its document, the variables it was given, and how to read its response as `D`. */
final class Request[D](
    val document: String,
    val name: Option[String],
    val variables: JsonObject,
    val decoder: Decoder[D]
) {
  override def toString: String = s"Request(${name.getOrElse("anonymous")}, ${variables.toJson.noSpaces})"
}

/** An operation whose types the macro wrote from the schema: `api.operation(document)`, for an [[Api]].
  *
  * `D` is the response: a [[Record]] with a member for each field the document selects. `Variables` lists the variables
  * the document declares, and `Sdl` is the schema, which is what lets a call be checked:
  * {{{
  * val pets = petshop.operation("{ pets { name } }")
  *
  * client.run(pets()).map(_.pets.map(_.name))
  * }}}
  * `pets()` compiles only if the document declares no variable that must be given a value.
  */
final class TypedOperation[Sdl <: String, Variables <: Tuple, D](
    val document: String,
    val name: Option[String],
    val decoder: Decoder[D]
) extends Dynamic {

  /** The type of the response, for a signature that needs to name it. */
  type Data = D

  /** `operation()`, for an operation that needs no variables. */
  inline def applyDynamic(inline method: String)(inline variables: Any*): Request[D] =
    ${ TypedMacro.requestWithout[Sdl, Variables, D]('this, 'variables) }

  override def toString: String = s"TypedOperation(${name.getOrElse("anonymous")})"
}
