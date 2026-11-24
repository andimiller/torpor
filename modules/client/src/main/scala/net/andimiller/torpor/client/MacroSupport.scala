package net.andimiller.torpor.client

import net.andimiller.torpor.schema.{Schema, SdlFragment}

import scala.quoted.*

/** What the client's macros share: reading a string and a schema at compile time. */
private[client] object MacroSupport {

  /** A string the compiler can read now: a literal. */
  def constant(expr: Expr[String])(using Quotes): Option[String] =
    expr.value

  /** The operation's text, or a compile error saying what it may be. */
  def document(expr: Expr[String])(using Quotes): String =
    constant(expr).getOrElse(quotes.reflect.report.errorAndAbort("the operation must be a string literal"))

  /** The schema an `Api` value's type names: the SDL itself, as a literal type. */
  def schema[Sdl <: String: Type](using Quotes): Schema = {
    import quotes.reflect.*
    TypeRepr.of[Sdl].dealias match {
      case ConstantType(StringConstant(sdl)) => schemaInSdl(sdl)
      case other                             => report.errorAndAbort(s"expected the schema as a literal type, found ${other.show}")
    }
  }

  /** The schema in the SDL, or a compile error. */
  def schemaInSdl(sdl: String)(using Quotes): Schema = parsed("schema", sdl)

  private def parsed(label: String, sdl: String)(using Quotes): Schema = {
    import quotes.reflect.*
    Schema.merge(List(SdlFragment(label, sdl))) match {
      case Left(errors)  => report.errorAndAbort(errors.toChain.toList.map(_.render).mkString("\n"))
      case Right(merged) => merged
    }
  }

  def schemaText(expr: Expr[String])(using Quotes): String =
    constant(expr).getOrElse(
      quotes.reflect.report.errorAndAbort("the schema must be a constant string: a literal, or an inline val")
    )
}
