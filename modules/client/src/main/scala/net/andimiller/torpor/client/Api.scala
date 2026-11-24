package net.andimiller.torpor.client

import io.circe.Decoder

import scala.quoted.*

/** A GraphQL API as its caller sees it: the schema, and the Scala type each leaf is read as.
  *
  * {{{
  * val petshop = Api
  *   .sdl(PetshopSchema.sdl)  // the schema as a literal or an inline val
  *   .read["Pet.id", PetId]   // one field
  *   .read["Int", Long]       // every field of a type
  *
  * val pets = petshop.operation("query Pets($first: Int) { pets(first: $first) { id name } }")
  * }}}
  *
  * `read` names a field as `Type.field` or a scalar or enum by its name, and takes the `Decoder` in scope for the Scala
  * type, or one passed explicitly. A field's own binding is used before its type's. Unbound, `Int` is `Int`, `Float` is
  * `Double`, `String` and `ID` are `String`, and any other scalar or enum is circe `Json`.
  *
  * The schema and the bindings are part of this value's type, which is where `operation` reads them from. So nothing
  * has to be in scope where an operation is stated but the `Api` itself.
  */
final class Api[Sdl <: String, Bindings <: Tuple] private (val decoders: Map[String, Decoder[?]]) {

  /** Reads the named field, scalar or enum as `A`. */
  def read[Name <: String & Singleton, A](using
      name: ValueOf[Name],
      decoder: Decoder[A]
  ): Api[Sdl, (Name, A) *: Bindings]                                                 =
    new Api(decoders + (name.value -> decoder))

  /** An operation typed from the schema: see [[TypedOperation]]. Its response is a [[Record]] with a member for each
    * field the document selects, and its variables are given by name.
    */
  transparent inline def operation(inline document: String): TypedOperation[?, ?, ?] =
    ${ TypedMacro.typed[Sdl, Bindings]('this, 'document) }

  override def toString: String = decoders.keys.toList.sorted.mkString("Api(", ", ", ")")
}

object Api {

  /** The API whose schema is the given SDL, as a literal or an `inline val`. A schema that does not parse, or that
    * defines a type twice, does not compile.
    */
  transparent inline def sdl(inline text: String): Api[?, EmptyTuple] = ${ ApiMacro.sdl('text) }

  /** Called by code `sdl` generates; not part of the API. */
  def unchecked[Sdl <: String]: Api[Sdl, EmptyTuple] = new Api(Map.empty)
}

private[client] object ApiMacro {

  def sdl(text: Expr[String])(using Quotes): Expr[Api[?, EmptyTuple]] = {
    import quotes.reflect.*
    val sdl = MacroSupport.schemaText(text)
    MacroSupport.schemaInSdl(sdl)
    ConstantType(StringConstant(sdl)).asType match {
      case '[s] => '{ Api.unchecked[s & String] }
    }
  }
}
