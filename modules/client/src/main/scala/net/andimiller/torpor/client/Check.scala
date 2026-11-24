package net.andimiller.torpor.client

import net.andimiller.torpor.schema.{
  ExecutableDocument,
  FieldSelection,
  FragmentSpread,
  InlineFragment,
  OperationDefinition,
  OperationKind,
  Schema,
  Selection,
  TypeKind,
  TypeRef
}

/** What an operation selects, as the macro that writes its response type reads it, and whether a value given for an
  * input fits the input's type.
  *
  * A field selected inside a fragment that only some types match, or behind `@skip` or `@include`, is conditional: the
  * response may not carry it.
  */
object Check {

  /** The concrete type of each object on the path from the root to a value. A selection in a fragment applies to some
    * worlds and not others, which is what decides whether a field is always in the response.
    */
  type World = List[String]

  /** A selection as written: on which type, and in which worlds it applies. */
  final case class Entry(written: String, selection: Selection, worlds: Set[World])

  /** What is selected on an object: its type, the selections, and every world a value of it may be in. */
  final case class Selections(parent: String, entries: List[Entry], worlds: Set[World])

  /** A selected field: where the schema declares it, its type, what is selected beneath it, the worlds it is in, and
    * whether a directive may leave it out.
    */
  final private case class Selected(
      owner: String,
      name: String,
      tpe: TypeRef,
      beneath: List[Entry],
      on: Set[World],
      skippable: Boolean
  )

  /** One response key of an object: the field the schema declares it as, its type, what is selected beneath it, the
    * worlds it is selected in, and whether the response may leave it out.
    */
  final case class Field(
      key: String,
      owner: String,
      name: String,
      tpe: TypeRef,
      beneath: Selections,
      on: Set[World],
      conditional: Boolean
  )

  /** A type as SDL writes it, such as `[ID!]!`. */
  def render(tpe: TypeRef): String = tpe match {
    case TypeRef.Named(name) => name
    case TypeRef.ListOf(of)  => s"[${render(of)}]"
    case TypeRef.NonNull(of) => s"${render(of)}!"
  }

  /** The inverse of [[render]]. */
  def parse(text: String): TypeRef =
    if (text.endsWith("!")) TypeRef.NonNull(parse(text.dropRight(1)))
    else if (text.startsWith("[")) TypeRef.ListOf(parse(text.drop(1).dropRight(1)))
    else TypeRef.Named(text)

  /** Scala types that can never be a whole input object. */
  private val primitives: Set[String] =
    Set("String", "Boolean", "Int", "Long", "Double", "Float", "BigInt", "BigDecimal", "Unit")

  private val leaves: Map[String, Set[String]] = Map(
    "String"  -> Set("String"),
    "Boolean" -> Set("Boolean"),
    "Int"     -> Set("Int", "Long", "BigInt"),
    "Float"   -> Set("Double", "Float", "BigDecimal")
  )

}

final private[client] class Check(schema: Schema, document: ExecutableDocument) {

  import Check.*

  private def leaf(name: String, shape: Shape, path: String): List[String] =
    shape match {
      case Shape.Trusted(_)         => Nil
      case Shape.Leaf(scala)        =>
        leaves.get(name).filterNot(_.contains(scala)).toList.map { allowed =>
          s"$path is a $name in the schema, so it is written as ${allowed.toList.sorted.mkString(" or ")}, not $scala"
        }
      // a literal is a string, unless the type is an enum: then each literal is one of its values
      case Shape.Literals(literals) =>
        schema.types.get(name).filter(_.kind == TypeKind.EnumType) match {
          case None             => leaf(name, Shape.Leaf("String"), path)
          case Some(definition) =>
            val values = definition.enumValues.map(_.name)
            literals.filterNot(values.contains).map { literal =>
              s"$path is the enum $name in the schema, but \"$literal\" is not one of its values: ${values.mkString(", ")}"
            }
        }
      case other                    =>
        List(s"$path is the scalar or enum $name in the schema, so it cannot be written as ${other.render}")
    }

  /** A value given for an input of type `tpe`, or the absence of one. An input that must be given a value is not an
    * `Option`; a list is a collection; an input object is a record with no field the schema lacks and every field the
    * schema requires.
    */
  def input(tpe: TypeRef, hasDefault: Boolean, value: Option[Shape], path: String): List[String] =
    (tpe, value) match {
      case (TypeRef.NonNull(_), None) if !hasDefault                    => List(s"$path must be given a value")
      case (_, None)                                                    => Nil
      case (TypeRef.NonNull(_), Some(Shape.Optional(_))) if !hasDefault =>
        List(s"$path must be given a value, so it cannot be an Option")
      case (_, Some(Shape.Optional(inner)))                             => inputValue(tpe, inner, path)
      case (_, Some(shape))                                             => inputValue(tpe, shape, path)
    }

  private def inputValue(tpe: TypeRef, shape: Shape, path: String): List[String] =
    (tpe, shape) match {
      case (_, Shape.Trusted(_))                     => Nil
      case (TypeRef.NonNull(of), other)              => inputValue(of, other, path)
      case (TypeRef.ListOf(of), Shape.Many(element)) => input(of, hasDefault = false, Some(element), s"$path[]")
      case (TypeRef.ListOf(_), other)                =>
        List(s"$path is a list in the schema, so it is written as a collection, not ${other.render}")
      case (TypeRef.Named(name), other)              =>
        schema.types.get(name) match {
          case Some(definition) if definition.kind == TypeKind.InputObjectType =>
            other match {
              case Shape.Record(_, fields)                 =>
                val declared = definition.fields.map(_.name).toSet
                val unknown  = fields.map(_._1).filterNot(declared).map { field =>
                  s"$path has a field '$field', which the input type $name does not have"
                }
                unknown ++ definition.fields.flatMap { field =>
                  input(field.tpe, field.defaultValue.isDefined, fields.toMap.get(field.name), s"$path.${field.name}")
                }
              // a type of the caller's own stands for the whole input object: its encoder decides what is sent
              case Shape.Leaf(scala) if !primitives(scala) => Nil
              case notRecord                               =>
                List(
                  s"$path is the input type $name in the schema, so it is written as a case class, not ${notRecord.render}"
                )
            }
          case _                                                               => leaf(name, other, path)
        }
    }

  /** The type an operation's response starts from, with the operation's selections on it. */
  def root(operation: OperationDefinition): Option[Selections] = {
    val name = operation.kind match {
      case OperationKind.Query        => schema.query
      case OperationKind.Mutation     => schema.mutation
      case OperationKind.Subscription => schema.subscription
    }
    name.map { root =>
      val world = Set(List(root))
      Selections(root, operation.selections.map(Entry(root, _, world)), world)
    }
  }

  /** The response keys selected, in the order the document first selects them.
    *
    * One response key selected in several fragments is one field, present wherever any of them applies. The validator
    * has already checked that every selection of it has the same type (5.3.2).
    */
  def fields(selections: Selections): List[Field] = {
    val selected = collect(selections.entries, skippable = false)
    val keys     = selected.map(_._1).distinct
    keys.map { key =>
      val occurrences = selected.collect { case (`key`, field) => field }
      val first       = occurrences.head
      val on          = occurrences.flatMap(_.on).toSet
      Field(
        key = key,
        owner = first.owner,
        name = first.name,
        tpe = first.tpe,
        beneath = Selections(first.tpe.named, occurrences.flatMap(_.beneath), within(on, first.tpe.named)),
        on = on,
        conditional = occurrences.forall(_.skippable) || !selections.worlds.subsetOf(on)
      )
    }
  }

  /** The response keys selected, with fragments expanded. A selection that applies in no world selects nothing. */
  private def collect(entries: List[Entry], skippable: Boolean): List[(String, Selected)] =
    entries.filter(_.worlds.nonEmpty).flatMap { case Entry(written, selection, worlds) =>
      val maybe =
        skippable || selection.directives.exists(directive => directive.name == "skip" || directive.name == "include")
      selection match {
        case field: FieldSelection if field.name == "__typename" =>
          val tpe = TypeRef.NonNull(TypeRef.Named("String"))
          List(field.responseKey -> Selected(written, "__typename", tpe, Nil, worlds, maybe))
        case field: FieldSelection                               =>
          schema.types.get(written).flatMap(_.fields.find(_.name == field.name)).toList.map { definition =>
            val beneath = within(worlds, definition.tpe.named)
            field.responseKey -> Selected(
              written,
              field.name,
              definition.tpe,
              field.selections.map(Entry(definition.tpe.named, _, beneath)),
              worlds,
              maybe
            )
          }
        case inline: InlineFragment                              =>
          val condition = inline.typeCondition.getOrElse(written)
          collect(inline.selections.map(Entry(condition, _, matching(worlds, condition))), maybe)
        case spread: FragmentSpread                              =>
          document.fragment(spread.name).toList.flatMap { fragment =>
            val condition = fragment.typeCondition
            collect(fragment.selections.map(Entry(condition, _, matching(worlds, condition))), maybe)
          }
      }
    }

  /** The worlds a fragment on `condition` applies to. */
  private def matching(worlds: Set[World], condition: String): Set[World] = {
    val members = possible(condition)
    worlds.filter(world => members(world.last))
  }

  /** The worlds beneath a field of the named type: each world, extended with each type the value can be. */
  private def within(worlds: Set[World], name: String): Set[World] =
    if (isObject(name)) worlds.flatMap(world => possible(name).map(world :+ _)) else Set.empty

  private def isObject(name: String): Boolean =
    schema.types.get(name).exists { tpe =>
      tpe.kind == TypeKind.ObjectType || tpe.kind == TypeKind.InterfaceType || tpe.kind == TypeKind.UnionType
    }

  /** The object types a value of the named type can be. */
  private def possible(name: String): Set[String] =
    schema.types.get(name) match {
      case Some(tpe) if tpe.kind == TypeKind.UnionType     => tpe.unionMembers.toSet
      // an interface may implement this one too, but a value is always an object
      case Some(tpe) if tpe.kind == TypeKind.InterfaceType =>
        schema.types.values.filter(t => t.kind == TypeKind.ObjectType && t.interfaces.contains(name)).map(_.name).toSet
      case _                                               => Set(name)
    }
}
