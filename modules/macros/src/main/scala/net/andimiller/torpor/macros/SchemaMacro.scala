package net.andimiller.torpor.macros

import net.andimiller.torpor.core.{FieldCoordinate, Sdl}
import net.andimiller.torpor.schema.{Schema, SdlFragment, TypeKind}

import scala.quoted.*

/** `schema"""…"""`: parse and validate the SDL at compile time, and emit its text plus every field coordinate.
  *
  * The coordinates are carried in the result's type: a refinement adds one member per type with one member per field.
  * That makes an unknown coordinate a compile error at the call site, without a second macro there.
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

    Schema.declarations(SdlFragment("<schema>", text)) match {
      case Left(errors)    => report.errorAndAbort(errors.toChain.toList.map(_.render).mkString("\n"))
      case Right(declared) =>
        val objectTypes = declared.filter(tpe => tpe.kind == TypeKind.ObjectType || tpe.kind == TypeKind.InterfaceType)
        val fields      = objectTypes.flatMap(tpe => tpe.fields.map(field => (tpe.name, field.name))).distinct.sorted

        // Sdl { val Query: Sdl.TypeFields { val character: FieldCoordinate.Of["Query", "character"] } … }
        val refined =
          fields.groupBy(_._1).toList.sortBy(_._1).foldLeft(TypeRepr.of[Sdl]) { case (sdl, (parent, typeFields)) =>
            val members = typeFields.foldLeft(TypeRepr.of[Sdl.TypeFields]) { case (inner, (_, field)) =>
              Refinement(
                inner,
                field,
                TypeRepr
                  .of[FieldCoordinate.Of]
                  .appliedTo(List(ConstantType(StringConstant(parent)), ConstantType(StringConstant(field))))
              )
            }
            Refinement(sdl, parent, members)
          }

        refined.asType match {
          case '[r] => '{ new Sdl(${ Expr(text) }).asInstanceOf[r & Sdl] }
        }
    }
  }
}
