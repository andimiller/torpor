package net.andimiller.torpor.client

import io.circe.{Decoder, Json, JsonObject}
import net.andimiller.torpor.schema.{Executable, TypeKind, TypeRef as GqlType}

import scala.quoted.*

/** The macros behind `Api.operation` and calling a typed operation. */
object TypedMacro {

  /** Types an operation from the schema: a [[Record]] refined with the fields the document selects, a decoder for it,
    * and the variables the document declares.
    *
    * A GraphQL type becomes a Scala type the way the server's `schema` macro types arguments: nullable is `Option`, a
    * list is `List`, an object is a nested record. A field selected in a fragment only some types match, or behind
    * `@skip` or `@include`, is an `Option` too. A leaf is read as the Scala type that holds it, as [[Api]] describes.
    */
  def typed[Sdl <: String: Type](document: Expr[String])(using Quotes): Expr[TypedOperation[?, ?, ?]] = {
    import quotes.reflect.*

    val schema = MacroSupport.schema[Sdl]
    val query  = MacroSupport.document(document)

    def literal(value: String): TypeRepr = ConstantType(StringConstant(value))

    val validated = Executable.validate(schema, query) match {
      case Left(errors) =>
        report.errorAndAbort(
          errors.toChain.toList.map(_.render).mkString("the operation does not fit the schema:\n  ", "\n  ", "")
        )
      case Right(valid) => valid
    }
    val check     = new Check(schema, validated.document)

    def tuple(elements: List[TypeRepr]): TypeRepr =
      elements.foldRight(TypeRepr.of[EmptyTuple])((element, rest) => TypeRepr.of[*:].appliedTo(List(element, rest)))

    /** The type a leaf is read as, and circe's decoder for it. */
    def leaf(name: String): (TypeRepr, Expr[Decoder[?]]) =
      name match {
        case "Int"           => (TypeRepr.of[Int], '{ Decoder.decodeInt })
        case "Float"         => (TypeRepr.of[Double], '{ Decoder.decodeDouble })
        case "Boolean"       => (TypeRepr.of[Boolean], '{ Decoder.decodeBoolean })
        case "String" | "ID" => (TypeRepr.of[String], '{ Decoder.decodeString })
        case _               => (TypeRepr.of[Json], '{ Decoder.decodeJson })
      }

    /** An `Option` of a type, read with its decoder when the value is there. */
    def optional(tpe: TypeRepr, decoder: Expr[Decoder[?]]): (TypeRepr, Expr[Decoder[?]]) =
      (TypeRepr.of[Option].appliedTo(tpe), '{ Decoder.decodeOption(using $decoder.asInstanceOf[Decoder[Any]]) })

    /** A record of the fields selected on an object. */
    def record(selections: Check.Selections, path: String): (TypeRepr, Expr[Decoder[?]]) = {
      val selected = check.fields(selections)
      val fields   = selected.map { field =>
        val (tpe, decoder) = value(field, field.tpe, field.conditional, s"$path.${field.key}")
        (field.key, tpe, decoder)
      }
      val refined  = fields.foldLeft(TypeRepr.of[Record]) { case (record, (key, tpe, _)) =>
        Refinement(record, key, tpe)
      }
      val pairs    = Expr.ofList(fields.map { case (key, _, decoder) => '{ (${ Expr(key) }, $decoder) } })
      (refined, '{ Record.decoder($pairs) })
    }

    def value(field: Check.Field, tpe: GqlType, conditional: Boolean, path: String): (TypeRepr, Expr[Decoder[?]]) =
      tpe match {
        case GqlType.NonNull(of) if !conditional => required(field, of, path)
        case other                               =>
          val (inner, decoder) = required(field, other, path)
          optional(inner, decoder)
      }

    def required(field: Check.Field, tpe: GqlType, path: String): (TypeRepr, Expr[Decoder[?]]) =
      tpe match {
        case GqlType.NonNull(of) => required(field, of, path)
        case GqlType.ListOf(of)  =>
          val (element, decoder) = value(field, of, conditional = false, s"$path[]")
          (TypeRepr.of[List].appliedTo(element), '{ Decoder.decodeList(using $decoder.asInstanceOf[Decoder[Any]]) })
        case GqlType.Named(name) =>
          schema.types.get(name).map(_.kind) match {
            case Some(TypeKind.ObjectType | TypeKind.InterfaceType | TypeKind.UnionType) =>
              record(field.beneath, path)
            case _                                                                       => leaf(name)
          }
      }

    val operation = validated.operation
    val root      = check.root(operation).getOrElse(report.errorAndAbort(s"the schema has no ${operation.kind.label} type"))

    val (data, decoder) = record(root, operation.kind.label)
    val variables       = tuple(operation.variables.map { variable =>
      TypeRepr
        .of[Tuple3]
        .appliedTo(
          List(
            literal(variable.name),
            literal(Check.render(variable.tpe)),
            ConstantType(BooleanConstant(variable.default.isDefined))
          )
        )
    })

    // built untyped and then given its type, which is what a `transparent inline` caller sees
    val untyped = '{
      new TypedOperation[String, Tuple, Any](
        ${ Expr(query) },
        ${ Expr(operation.name) },
        $decoder.asInstanceOf[Decoder[Any]]
      )
    }
    TypeRepr.of[TypedOperation].appliedTo(List(TypeRepr.of[Sdl], variables, data)).asType match {
      case '[t] => '{ $untyped.asInstanceOf[t] }.asExprOf[TypedOperation[?, ?, ?]]
    }
  }

  /** `operation()`: allowed when no variable must be given a value. */
  def requestWithout[Sdl <: String: Type, Variables <: Tuple: Type, D: Type](
      operation: Expr[TypedOperation[Sdl, Variables, D]],
      supplied: Expr[Seq[Any]]
  )(using Quotes): Expr[Request[D]] = {
    import quotes.reflect.*

    supplied match {
      case Varargs(values) if values.nonEmpty =>
        report.errorAndAbort("give an operation's variables by name: operation(name = value, …)")
      case _                                  => ()
    }

    val problems = declaredVariables[Variables].collect {
      case (name, tpe, hasDefault) if tpe.isNonNull && !hasDefault => s"$$$name must be given a value"
    }
    if (problems.nonEmpty)
      report.errorAndAbort(problems.mkString("the variables do not fit the operation:\n  ", "\n  ", ""))

    '{ new Request[D]($operation.document, $operation.name, JsonObject.empty, $operation.decoder) }
  }

  private def declaredVariables[V <: Tuple: Type](using Quotes): List[(String, GqlType, Boolean)] = {
    import quotes.reflect.*

    def each(tpe: TypeRepr): List[(String, GqlType, Boolean)] = tpe.dealias match {
      case AppliedType(_, List(AppliedType(_, List(name, declared, default)), rest)) =>
        (name.dealias, declared.dealias, default.dealias) match {
          case (
                ConstantType(StringConstant(variable)),
                ConstantType(StringConstant(gql)),
                ConstantType(BooleanConstant(hasDefault))
              ) =>
            (variable, Check.parse(gql), hasDefault) :: each(rest)
          case _ => each(rest)
        }
      case _                                                                         => Nil
    }

    each(TypeRepr.of[V])
  }
}
