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

/** What an operation selects, as the macro that writes its response type reads it.
  *
  * A field behind `@skip` or `@include` is conditional: the response may not carry it.
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

}

final private[client] class Check(schema: Schema, document: ExecutableDocument) {

  import Check.*

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
    val selected = collect(selections.entries)
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

  /** The response keys selected. A selection that applies in no world selects nothing. */
  private def collect(entries: List[Entry]): List[(String, Selected)] =
    entries.filter(_.worlds.nonEmpty).flatMap { case Entry(written, selection, worlds) =>
      val maybe = selection.directives.exists(directive => directive.name == "skip" || directive.name == "include")
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
        case _: InlineFragment | _: FragmentSpread               => Nil
      }
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
