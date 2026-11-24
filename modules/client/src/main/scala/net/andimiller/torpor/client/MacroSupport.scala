package net.andimiller.torpor.client

import cats.syntax.all.*
import net.andimiller.torpor.schema.{Schema, SdlFragment}

import scala.quoted.*

/** What the client's macros share: reading a string and a schema at compile time. */
private[client] object MacroSupport {

  /** A string the compiler can read now: a literal, a constant such as an `inline val`, or literals and constants put
    * together with `+`, with `s"…"`, or trimmed with `stripMargin`. That lets several operations share a selection.
    */
  def constant(expr: Expr[String])(using Quotes): Option[String] = {
    import quotes.reflect.*
    expr match {
      case Expr(value)                                                     => Some(value)
      case '{ ($left: String) + ($right: String) }                         => (constant(left), constant(right)).mapN(_ + _)
      case '{ scala.Predef.augmentString($text).stripMargin }              => constant(text).map(_.stripMargin)
      case '{ StringContext(${ Varargs(parts) }*).s(${ Varargs(args) }*) } =>
        val texts  = parts.toList.traverse(constant).map(_.map(StringContext.processEscapes))
        val values = args.toList.traverse {
          case '{ $arg: String } => constant(arg)
          case _                 => None
        }
        (texts, values).mapN((texts, values) => texts.zipAll(values, "", "").map(_ + _).mkString)
      case other                                                           =>
        other.asTerm.tpe.widenTermRefByName.dealias match {
          case ConstantType(StringConstant(value)) => Some(value)
          case _                                   => None
        }
    }
  }

  /** The operation's text, or a compile error saying what it may be built from. */
  def document(expr: Expr[String])(using Quotes): String =
    constant(expr).getOrElse(
      quotes.reflect.report.errorAndAbort(
        "the operation must be a constant string: a literal, or literals and inline vals joined with s\"…\" or +"
      )
    )

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
