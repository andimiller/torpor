package net.andimiller.torpor.macros

import net.andimiller.torpor.core.{FieldCoordinate, Sdl}
import net.andimiller.torpor.schema.{Schema, SdlFragment}

import scala.quoted.*

/** `schema"""…"""`: parse and validate the SDL at compile time, and emit its text plus every field coordinate.
  *
  * Unlike a coordinate-checking macro, this needs no second inline macro at the call site: the map is an ordinary
  * value, so `shop.coordinate("Character", "corporation")` is a plain lookup and the SDL is checked once, where it is
  * written.
  */
object SchemaMacro {

  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[Sdl] = {
    import quotes.reflect.*

    args match {
      case Varargs(items) if items.nonEmpty =>
        report.errorAndAbort("schema\"...\" must be a literal: interpolation would move parsing back to runtime")
      case _                                => ()
    }

    val parts = sc match {
      case '{ StringContext(${ Varargs(parts) }*) } =>
        parts.toList.map {
          case Expr(text: String) => text
          case _                  => report.errorAndAbort("schema\"...\" must be a literal string")
        }
      case other                                    => report.errorAndAbort(s"schema\"...\" must be a literal string, found: ${other.show}")
    }

    // stripMargin here so the `|` layout works without a trailing call
    val text = parts.mkString.stripMargin

    Schema.coordinates(SdlFragment("<schema>", text)) match {
      case Left(errors)       => report.errorAndAbort(errors.toChain.toList.map(_.render).mkString("\n"))
      case Right(coordinates) =>
        val entries = coordinates.map { coordinate =>
          '{
            (${ Expr(coordinate.parentType) }, ${ Expr(coordinate.field) }) ->
              FieldCoordinate(${ Expr(coordinate.parentType) }, ${ Expr(coordinate.field) })
          }
        }
        '{ Sdl(${ Expr(text) }, Map(${ Varargs(entries) }*)) }
    }
  }
}
