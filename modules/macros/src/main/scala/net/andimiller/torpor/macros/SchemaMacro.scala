package net.andimiller.torpor.macros

import io.circe.Json
import net.andimiller.torpor.core.{Arguments, FieldCoordinate, Scalar, Sdl}
import net.andimiller.torpor.schema.{
  InputValueDef,
  Schema,
  SdlFragment,
  TypeDef as SdlTypeDef,
  TypeKind,
  TypeRef as SdlType
}

import scala.quoted.*

/** `schema"""…"""`: parse and validate the SDL at compile time, and emit its text plus every field coordinate.
  *
  * The coordinates are carried in the result's type: a refinement adds one member per type with one member per field.
  * That makes an unknown coordinate a compile error at the call site, without a second macro there, and types each
  * coordinate with the field's arguments.
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

    def tuple(elements: List[TypeRepr]): TypeRepr =
      elements.foldRight(TypeRepr.of[EmptyTuple])((element, rest) => TypeRepr.of[*:].appliedTo(List(element, rest)))

    def literal(value: String): TypeRepr = ConstantType(StringConstant(value))

    /** One argument's type, twice: as `args.name` reads it, and as `Arguments.decoder` reads it. */
    final case class Typed(member: TypeRepr, decoded: TypeRepr) {
      def in(wrapper: TypeRepr): Typed = Typed(wrapper.appliedTo(member), wrapper.appliedTo(decoded))
    }

    object Typed {
      def same(tpe: TypeRepr): Typed = Typed(tpe, tpe)

      /** `Arguments[Spec] { val name: … }` for args, and the bare `Arguments[Spec]` for the decoder. */
      def record(fields: List[(String, Typed)]): Typed = {
        val spec = tuple(fields.map { case (name, typed) =>
          TypeRepr.of[Tuple2].appliedTo(List(literal(name), typed.decoded))
        })
        val base = TypeRepr.of[Arguments].appliedTo(spec)
        Typed(fields.foldLeft(base) { case (record, (name, typed)) => Refinement(record, name, typed.member) }, base)
      }
    }

    /** GraphQL argument types to Scala types, from what one fragment declares plus the `Scalar` bindings in scope. */
    def argumentsType(declared: List[SdlTypeDef], args: List[InputValueDef]): TypeRepr = {
      val byName = declared.groupBy(_.name).view.mapValues(_.head).toMap

      def bound(scalar: String): Option[TypeRepr] =
        Implicits.search(
          TypeRepr.of[Scalar].appliedTo(List(ConstantType(StringConstant(scalar)), TypeBounds.empty))
        ) match {
          case found: ImplicitSearchSuccess =>
            found.tree.tpe.widen.baseType(TypeRepr.of[Scalar].typeSymbol) match {
              case AppliedType(_, List(_, scala)) => Some(scala)
              case _                              => None
            }
          case _                            => None
        }

      // nullable is `Option`, a list is `List`; `seen` stops a recursive input type from expanding forever
      def value(ref: SdlType, seen: Set[String]): Typed = ref match {
        case SdlType.NonNull(of) => nonNull(of, seen)
        case other               => nonNull(other, seen).in(TypeRepr.of[Option])
      }

      def nonNull(ref: SdlType, seen: Set[String]): Typed = ref match {
        case SdlType.NonNull(of) => nonNull(of, seen)
        case SdlType.ListOf(of)  => value(of, seen).in(TypeRepr.of[List])
        case SdlType.Named(name) => named(name, seen)
      }

      def named(name: String, seen: Set[String]): Typed = name match {
        case "Int"     => Typed.same(TypeRepr.of[Int])
        case "Float"   => Typed.same(TypeRepr.of[Double])
        case "String"  => Typed.same(TypeRepr.of[String])
        case "Boolean" => Typed.same(TypeRepr.of[Boolean])
        case "ID"      => Typed.same(bound(name).getOrElse(TypeRepr.of[String]))
        case _         =>
          byName.get(name) match {
            // `"DOG" | "CAT"` for args; the decoder cannot list a union's members, so it reads `OneOf[("DOG", "CAT")]`
            case Some(tpe) if tpe.kind == TypeKind.EnumType && tpe.enumValues.nonEmpty =>
              Typed(
                member = tpe.enumValues.map(literal).reduce(OrType(_, _)),
                decoded = TypeRepr.of[Arguments.OneOf].appliedTo(tuple(tpe.enumValues.map(literal)))
              )
            case Some(tpe) if tpe.kind == TypeKind.InputObjectType && !seen(name)      =>
              Typed.record(tpe.fields.map(field => field.name -> value(field.tpe, seen + name)))
            // custom scalars, recursive inputs, and types another fragment declares
            case _                                                                     => Typed.same(bound(name).getOrElse(TypeRepr.of[Json]))
          }
      }

      Typed
        .record(args.map { arg =>
          // an argument with a default may be left out, and the planner does not fill defaults in
          val tpe =
            if (arg.hasDefault && arg.tpe.isNonNull) nonNull(arg.tpe, Set.empty).in(TypeRepr.of[Option])
            else value(arg.tpe, Set.empty)
          arg.name -> tpe
        })
        .member
    }

    // stripMargin here so the `|` layout works without a trailing call
    val text = parts.mkString.stripMargin

    Schema.declarations(SdlFragment("<schema>", text)) match {
      case Left(errors)    => report.errorAndAbort(errors.toChain.toList.map(_.render).mkString("\n"))
      case Right(declared) =>
        val objectTypes = declared.filter(tpe => tpe.kind == TypeKind.ObjectType || tpe.kind == TypeKind.InterfaceType)
        val fields      = objectTypes
          .flatMap(tpe => tpe.fields.map(field => (tpe.name, field.name, argumentsType(declared, field.args))))
          .distinctBy { case (parent, field, _) => (parent, field) }
          .sortBy { case (parent, field, _) => s"$parent.$field" }

        // Sdl { val Query: Sdl.TypeFields { val character: FieldCoordinate.Of["Query", "character", …] } … }
        val refined     =
          fields.groupBy(_._1).toList.sortBy(_._1).foldLeft(TypeRepr.of[Sdl]) { case (sdl, (parent, typeFields)) =>
            val members = typeFields.foldLeft(TypeRepr.of[Sdl.TypeFields]) { case (inner, (_, field, args)) =>
              Refinement(
                inner,
                field,
                TypeRepr
                  .of[FieldCoordinate.Of]
                  .appliedTo(List(literal(parent), literal(field), args))
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
