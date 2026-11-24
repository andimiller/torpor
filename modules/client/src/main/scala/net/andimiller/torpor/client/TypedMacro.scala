package net.andimiller.torpor.client

import io.circe.{Decoder, Encoder, Json, JsonObject}
import net.andimiller.torpor.schema.{Executable, TypeKind, TypeRef as GqlType}

import scala.quoted.*

/** The macros behind `Api.operation` and calling a typed operation. */
object TypedMacro {

  /** Types an operation from the schema: a [[Record]] refined with the fields the document selects, a decoder for it,
    * and the variables the document declares.
    *
    * A GraphQL type becomes a Scala type the way the server's `schema` macro types arguments: nullable is `Option`, a
    * list is `List`, an object is a nested record. A field selected in a fragment only some types match, or behind
    * `@skip` or `@include`, is an `Option` too. A leaf is read as the type the [[Api]] binds it to, looked up first for
    * the field (`Pet.id`) and then for its type (`ID`).
    */
  def typed[Sdl <: String: Type, Bindings <: Tuple: Type](
      api: Expr[Api[Sdl, Bindings]],
      document: Expr[String]
  )(using Quotes): Expr[TypedOperation[?, ?, ?]] = {
    import quotes.reflect.*

    val schema = MacroSupport.schema[Sdl]
    val query  = MacroSupport.document(document)

    def literal(value: String): TypeRepr = ConstantType(StringConstant(value))

    val bindings: Map[String, TypeRepr] = {
      def each(tpe: TypeRepr): List[(String, TypeRepr)] = tpe.dealias match {
        case AppliedType(_, List(AppliedType(_, List(name, bound)), rest)) =>
          name.dealias match {
            case ConstantType(StringConstant(value)) => (value -> bound) :: each(rest)
            case _                                   => each(rest)
          }
        case _                                                             => Nil
      }
      // a later `read` of the same name comes first in the type, and wins
      each(TypeRepr.of[Bindings]).reverse.toMap
    }

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

    /** A union of string literals, such as `"DOG" | "CAT"`, and a decoder that accepts only those. */
    def oneOf(values: List[String], what: String): (TypeRepr, Expr[Decoder[?]]) = {
      val tpe      = values.map(value => literal(value)).reduceLeft[TypeRepr](OrType(_, _))
      val expected = Expr(values)
      val decoder  = '{
        Decoder.decodeString.emap { value =>
          if ($expected.contains(value)) Right(value)
          else Left(s"$value is not a value of ${${ Expr(what) }}: ${$expected.mkString(", ")}")
        }
      }
      (tpe, decoder)
    }

    /** The type a leaf is read as, and its decoder: the one the API was given for a binding, or circe's own. An enum
      * unbound is the union of its values as literals.
      */
    def leaf(field: Check.Field, name: String): (TypeRepr, Expr[Decoder[?]]) =
      List(s"${field.owner}.${field.name}", name).find(bindings.contains) match {
        case Some(key) => (bindings(key), '{ $api.decoders(${ Expr(key) }) })
        case None      =>
          (name, schema.types.get(name)) match {
            case ("Int", _)                                                    => (TypeRepr.of[Int], '{ Decoder.decodeInt })
            case ("Float", _)                                                  => (TypeRepr.of[Double], '{ Decoder.decodeDouble })
            case ("Boolean", _)                                                => (TypeRepr.of[Boolean], '{ Decoder.decodeBoolean })
            case ("String" | "ID", _)                                          => (TypeRepr.of[String], '{ Decoder.decodeString })
            case (_, Some(definition)) if definition.kind == TypeKind.EnumType =>
              oneOf(definition.enumValues.map(_.name), s"enum $name")
            case _                                                             => (TypeRepr.of[Json], '{ Decoder.decodeJson })
          }
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
            case _                                                                       => leaf(field, name)
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

  /** `operation(name = value, …)`: each value against the variable it is given for, then the request. */
  def request[Sdl <: String: Type, Variables <: Tuple: Type, D: Type](
      operation: Expr[TypedOperation[Sdl, Variables, D]],
      supplied: Expr[Seq[(String, Any)]]
  )(using Quotes): Expr[Request[D]] = {
    requestOf[Sdl, Variables, D](operation, named(supplied))
  }

  private def requestOf[Sdl <: String: Type, Variables <: Tuple: Type, D: Type](using q: Quotes)(
      operation: Expr[TypedOperation[Sdl, Variables, D]],
      values: List[(String, q.reflect.TypeRepr, Expr[Option[Json]])]
  ): Expr[Request[D]] = {
    import q.reflect.*

    val schema   = MacroSupport.schema[Sdl]
    val check    = new Check(schema, net.andimiller.torpor.schema.ExecutableDocument(Nil, Nil))
    val declared = declaredVariables[Variables]

    val unknown  = values.map(_._1).filterNot(name => declared.exists(_._1 == name)).map { name =>
      s"the operation declares no variable '$$$name'"
    }
    val problems = unknown ++ declared.flatMap { case (name, tpe, hasDefault) =>
      check.input(tpe, hasDefault, values.find(_._1 == name).map(value => MacroSupport.shape(value._2)), s"$$$name")
    }
    if (problems.nonEmpty)
      report.errorAndAbort(problems.mkString("the variables do not fit the operation:\n  ", "\n  ", ""))

    val fields = Expr.ofList(values.map { case (name, _, json) => '{ (${ Expr(name) }, $json) } })
    '{
      new Request[D](
        $operation.document,
        $operation.name,
        // a variable given as `None` has no value, which is what leaving it out of the call means
        JsonObject.fromIterable($fields.collect { case (name, Some(json)) => name -> json }),
        $operation.decoder
      )
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
    requestOf[Sdl, Variables, D](operation, Nil)
  }

  /** Named arguments as `(name, the value's own type, the value encoded)`, where a `None` encodes to nothing. */
  private def named(using
      q: Quotes
  )(supplied: Expr[Seq[(String, Any)]]): List[(String, q.reflect.TypeRepr, Expr[Option[Json]])] = {
    import q.reflect.*

    val pairs = supplied match {
      case Varargs(pairs) => pairs.toList
      case _              => report.errorAndAbort("give each value by name: name = value")
    }
    pairs.map {
      case '{ ($name: String, $value: Any) } =>
        val key = name.value.filter(_.nonEmpty).getOrElse(report.errorAndAbort("give each value by name: name = value"))
        // the pair is typed `(String, Any)`, so the value's own type is read off its tree; a literal keeps its type,
        // which is how `"DOG"` is checked against an enum
        val tpe = unascribed(value.asTerm).tpe.widenTermRefByName
        (key, tpe, encoded(key, tpe.widen, value))
      case other                             =>
        report.errorAndAbort(s"give each value by name: name = value (found ${other.show})")
    }
  }

  /** The value as JSON, with the `Encoder` in scope for its type, or nothing for an `Option` that is `None`. */
  private def encoded(using q: Quotes)(key: String, tpe: q.reflect.TypeRepr, value: Expr[Any]): Expr[Option[Json]] = {
    import q.reflect.*

    def encoder[T: Type]: Expr[Encoder[T]] =
      Expr.summon[Encoder[T]].getOrElse {
        report.errorAndAbort(s"'$key' is a ${TypeRepr.of[T].show}, which has no Encoder in scope")
      }

    if (tpe <:< TypeRepr.of[Option[Any]])
      tpe.baseType(TypeRepr.of[Option[Any]].typeSymbol) match {
        case AppliedType(_, List(element)) =>
          element.asType match {
            case '[t] => '{ ${ value.asExprOf[Option[t]] }.map(held => ${ encoder[t] }(held)) }
          }
        case _                             => report.errorAndAbort(s"'$key' is a ${tpe.show}, which is not an Option of anything")
      }
    else
      tpe.asType match {
        case '[t] => '{ Some(${ encoder[t] }(${ value.asExprOf[t] })) }
      }
  }

  private def unascribed(using q: Quotes)(term: q.reflect.Term): q.reflect.Term = {
    import q.reflect.*
    term match {
      case Inlined(_, Nil, inner) => unascribed(inner)
      case Typed(inner, _)        => unascribed(inner)
      case other                  => other
    }
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
