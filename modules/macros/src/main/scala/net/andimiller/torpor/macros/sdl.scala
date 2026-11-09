package net.andimiller.torpor.macros

import net.andimiller.torpor.schema.{Schema, SdlFragment}

import scala.quoted.*

/** `sdl"""…"""`, parsed and checked when the code compiles instead of when the server starts.
  *
  * It produces a plain `String`, so it is a drop-in for a bare triple-quoted literal: `Source(id, sdl"""…""",
  * resolvers)`. Runtime and compile time run the same checks (`Schema.check`), so nothing is accepted here that startup
  * would reject.
  */
object SdlMacro {

  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[String] = {
    import quotes.reflect.*

    args match {
      case Varargs(items) if items.nonEmpty =>
        report.errorAndAbort("sdl\"...\" must be a literal: interpolation would move parsing back to runtime")
      case _                                => ()
    }

    val parts = sc match {
      case '{ StringContext(${ Varargs(parts) }*) } =>
        parts.toList.map {
          case Expr(text: String) => text
          case _                  => report.errorAndAbort("sdl\"...\" must be a literal string")
        }
      case other                                    => report.errorAndAbort(s"sdl\"...\" must be a literal string, found: ${other.show}")
    }

    // stripMargin inside the macro so the usual `|` layout works without a trailing .stripMargin
    val text   = parts.mkString.stripMargin
    val errors = Schema.check(SdlFragment("<sdl>", text))

    if (errors.nonEmpty) report.errorAndAbort(errors.map(_.render).mkString("\n"))

    Expr(text)
  }
}
