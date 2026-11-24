package net.andimiller.torpor.client

import scala.quoted.*

/** A GraphQL API as its caller sees it: the schema its operations are typed from.
  *
  * {{{
  * val petshop = Api.sdl(PetshopSchema.sdl) // the schema as a literal or an inline val
  *
  * val pets = petshop.operation("{ pets { id name } }")
  * }}}
  *
  * A leaf is read as the Scala type that holds it: `Int` is `Int`, `Float` is `Double`, `String` and `ID` are `String`,
  * and any other scalar or enum is circe `Json`.
  *
  * The schema is part of this value's type, which is where `operation` reads it from. So nothing has to be in scope
  * where an operation is stated but the `Api` itself.
  */
final class Api[Sdl <: String] private () {

  /** An operation typed from the schema: see [[TypedOperation]]. Its response is a [[Record]] with a member for each
    * field the document selects.
    */
  transparent inline def operation(inline document: String): TypedOperation[?, ?, ?] =
    ${ TypedMacro.typed[Sdl]('document) }

  override def toString: String = "Api()"
}

object Api {

  /** The API whose schema is the given SDL, as a literal or an `inline val`. A schema that does not parse, or that
    * defines a type twice, does not compile.
    */
  transparent inline def sdl(inline text: String): Api[?] = ${ ApiMacro.sdl('text) }

  /** Called by code `sdl` generates; not part of the API. */
  def unchecked[Sdl <: String]: Api[Sdl] = new Api
}

private[client] object ApiMacro {

  def sdl(text: Expr[String])(using Quotes): Expr[Api[?]] = {
    import quotes.reflect.*
    val sdl = MacroSupport.schemaText(text)
    MacroSupport.schemaInSdl(sdl)
    ConstantType(StringConstant(sdl)).asType match {
      case '[s] => '{ Api.unchecked[s & String] }
    }
  }
}
