package net.andimiller.torpor.client

import cats.syntax.all.*
import net.andimiller.torpor.schema.{Schema, SdlFragment}

import scala.quoted.*

/** What the client's macros share: reading a string and a schema at compile time, and the shape of a Scala type. */
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

  // collections that are not an `Iterable`, by the name of their class
  private val collections = Set("NonEmptyList", "NonEmptyVector", "NonEmptyChain", "NonEmptySeq", "Chain")

  /** The string literals a type is a union of, if it is one: `"DOG"`, or `"DOG" | "CAT"`. */
  def literals(using q: Quotes)(tpe: q.reflect.TypeRepr): Option[List[String]] = {
    import q.reflect.*
    tpe.dealias match {
      case ConstantType(StringConstant(value)) => Some(List(value))
      case OrType(left, right)                 => (literals(left), literals(right)).mapN(_ ++ _)
      case _                                   => None
    }
  }

  /** The shape of a type: see [[Shape]]. A value class is the shape of the one value it wraps, and an opaque type is
    * taken on trust: its own codec decides.
    */
  def shape(using q: Quotes)(tpe: q.reflect.TypeRepr): Shape = {
    import q.reflect.*

    val optionSymbol   = TypeRepr.of[Option[Any]].typeSymbol
    val iterableSymbol = TypeRepr.of[Iterable[Any]].typeSymbol

    def of(tpe: TypeRepr, within: Set[String]): Shape = {
      val dealiased = tpe.dealias
      val symbol    = dealiased.typeSymbol
      dealiased match {
        case _ if literals(dealiased).isDefined                                                       =>
          Shape.Literals(literals(dealiased).get)
        // an opaque type cannot be seen through from here, so its own codec decides what it writes
        case _ if symbol.flags.is(Flags.Opaque)                                                       =>
          Shape.Trusted(symbol.name)
        // a value class wraps one value, such as an id: it is written as that value, by its own codec
        case _
            if symbol.isClassDef && symbol.flags.is(Flags.Case) && dealiased <:< TypeRepr.of[AnyVal] &&
              symbol.caseFields.size == 1 =>
          of(dealiased.memberType(symbol.caseFields.head), within)
        // `Some(1)` and `None` are options too
        case _ if dealiased <:< TypeRepr.of[Option[Any]]                                              =>
          dealiased.baseType(optionSymbol) match {
            case AppliedType(_, List(of1)) => Shape.Optional(of(of1, within))
            case _                         => Shape.Optional(Shape.Leaf("Nothing"))
          }
        case AppliedType(_, List(of1)) if collections(symbol.name)                                    =>
          Shape.Many(of(of1, within))
        case _ if dealiased <:< TypeRepr.of[Iterable[Any]] && !(dealiased <:< TypeRepr.of[Map[?, ?]]) =>
          dealiased.baseType(iterableSymbol) match {
            case AppliedType(_, List(of1)) => Shape.Many(of(of1, within))
            case _                         => Shape.Leaf(symbol.name)
          }
        // a sealed trait, such as `Json`, is never an input object
        case _ if symbol.isClassDef && symbol.flags.is(Flags.Sealed) && !symbol.flags.is(Flags.Case)  =>
          Shape.Sealed(symbol.name, symbol.children.map(child => of(child.typeRef, within + symbol.fullName)))
        // a recursive case class is checked once: beneath itself it is taken on trust
        case _ if symbol.isClassDef && symbol.flags.is(Flags.Case) && !within(symbol.fullName)        =>
          Shape.Record(
            symbol.name,
            symbol.caseFields.map(field => field.name -> of(dealiased.memberType(field), within + symbol.fullName))
          )
        case _                                                                                        =>
          Shape.Leaf(symbol.name)
      }
    }

    of(tpe, Set.empty)
  }
}
