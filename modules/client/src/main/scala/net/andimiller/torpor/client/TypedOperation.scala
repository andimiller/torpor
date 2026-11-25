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
  * val pets = petshop.operation("query Pets($first: Int) { pets(first: $first) { name } }")
  *
  * client.run(pets(first = Some(2))).map(_.pets.map(_.name))
  * }}}
  * `pets(first = …)` compiles only if `first` is a variable the document declares and its value fits the variable's
  * type. A variable that must be given a value cannot be left out. One given as `None` is left out, and one given as
  * `null` is sent as null, which a nullable input with a default tells apart.
  */
final class TypedOperation[Sdl <: String, Variables <: Tuple, D](
    val document: String,
    val name: Option[String],
    val decoder: Decoder[D]
) extends Dynamic {

  /** The type of the response, for a signature that needs to name it. */
  type Data = D

  inline def applyDynamicNamed(inline method: String)(inline variables: (String, Any)*): Request[D] =
    ${ TypedMacro.request[Sdl, Variables, D]('this, 'variables) }

  /** `operation()`, for an operation that needs no variables. */
  inline def applyDynamic(inline method: String)(inline variables: Any*): Request[D] =
    ${ TypedMacro.requestWithout[Sdl, Variables, D]('this, 'variables) }

  override def toString: String = s"TypedOperation(${name.getOrElse("anonymous")})"
}
