package net.andimiller.torpor.schema

import cats.data.NonEmptyChain
import SangriaBridge.{Extension, Parsed, Root, RootOperation}

/** The merged schema: one type map, plus the root operation type names. */
final case class Schema(
    types: Map[String, TypeDef],
    query: Option[String],
    mutation: Option[String],
    subscription: Option[String]
) {
  def typeDef(name: String): Option[TypeDef] = types.get(name)

  def rootType: Option[TypeDef] = query.flatMap(types.get)

  def rootFields: List[FieldDef] = rootType.map(_.fields).getOrElse(Nil)
}

object Schema {

  /** Merge SDL fragments into one schema.
    *
    * Rules (v1):
    *   - a type is owned by exactly one source; a second definition is an error
    *   - other sources may extend it; extending an undefined type is an error, except for the root object types, which
    *     are synthesized so that every source can contribute root fields with `extend type Query`
    *   - two declarations of the same field on one type is an error, whichever side they come from
    *   - errors accumulate: one run reports every problem it found, not the first
    */
  private val conventionalRoots: Set[String] = Set("Query", "Mutation", "Subscription")

  /** Parse one fragment and report every problem in it. Exposed so the compile-time `sdl` interpolator runs exactly the
    * checks startup runs, instead of a second parser with its own opinions.
    */
  def check(fragment: SdlFragment): List[SchemaError] =
    SangriaBridge.parse(fragment) match {
      case Left(errors) => errors.toChain.toList
      case Right(_)     => Nil
    }

  /** Every type a fragment defines or extends, validated, as written in that fragment alone. The `schema` macro types
    * field arguments from this, so it only sees enums, input objects and scalars the same fragment declares.
    */
  def declarations(fragment: SdlFragment): Either[NonEmptyChain[SchemaError], List[TypeDef]] =
    SangriaBridge.parse(fragment).map(parsed => parsed.types ++ parsed.extensions.map(_.tpe))

  /** The fields a fragment's source must resolve, as `Type.field`: every field it adds with `extend type`, and every
    * field of a root type it declares. The `schema` macro records these in the SDL's type, so assembling sources can
    * check them when the code compiles; startup applies the same rule to the merged schema.
    */
  def required(fragment: SdlFragment): Either[NonEmptyChain[SchemaError], List[String]] =
    SangriaBridge.parse(fragment).map { parsed =>
      val roots      = parsed.roots.map(_.tpe).toSet ++ conventionalRoots
      val extended   = parsed.extensions.map(_.tpe)
      val rootFields = (parsed.types ++ extended).filter(tpe => roots(tpe.name))
      (extended ++ rootFields).flatMap(tpe => tpe.fields.map(field => s"${tpe.name}.${field.name}")).distinct.sorted
    }

  def merge(fragments: List[SdlFragment]): Either[NonEmptyChain[SchemaError], Schema] = {
    val results     = fragments.map(SangriaBridge.parse)
    val parseErrors = results.collect { case Left(errs) => errs.toChain.toVector }.flatten
    val parsed      = results.collect { case Right(p) => p }

    val explicitRoots      = collectRoots(parsed)
    val rootErrors         = explicitRoots.errors
    val synthesizableRoots = explicitRoots.byOp.values.map(_.tpe).toSet ++ conventionalRoots

    val (defined, definitionErrors)   = mergeDefinitions(parsed)
    val (extended, extensionErrors)   = applyExtensions(defined, parsed, synthesizableRoots)
    val (roots, rootResolutionErrors) = resolveRoots(extended, explicitRoots.byOp)

    val errors = NonEmptyChain.fromSeq(
      parseErrors ++ rootErrors ++ definitionErrors ++ extensionErrors ++ rootResolutionErrors
    )

    errors match {
      case Some(nec) => Left(nec)
      case None      =>
        Right(
          Schema(
            extended,
            query = roots.get(RootOperation.Query),
            mutation = roots.get(RootOperation.Mutation),
            subscription = roots.get(RootOperation.Subscription)
          )
        )
    }
  }

  private final case class RootDeclarations(byOp: Map[RootOperation, Root], errors: Vector[SchemaError])

  private def collectRoots(parsed: List[Parsed]): RootDeclarations =
    parsed
      .flatMap(_.roots)
      .foldLeft(RootDeclarations(Map.empty, Vector.empty)) { case (acc, root) =>
        acc.byOp.get(root.op) match {
          case Some(existing) if existing.tpe == root.tpe => acc
          case Some(existing)                             =>
            acc.copy(errors =
              acc.errors :+ SchemaError.RootConflict(
                root.op.label,
                existing.tpe,
                existing.origin,
                root.tpe,
                root.origin
              )
            )
          case None                                       => acc.copy(byOp = acc.byOp.updated(root.op, root))
        }
      }

  private def mergeDefinitions(parsed: List[Parsed]): (Map[String, TypeDef], Vector[SchemaError]) =
    parsed.foldLeft((Map.empty[String, TypeDef], Vector.empty[SchemaError])) { case ((types, errors), p) =>
      p.types.foldLeft((types, errors)) { case ((acc, errs), tpe) =>
        acc.get(tpe.name) match {
          case Some(existing) =>
            (acc, errs :+ SchemaError.DuplicateType(tpe.name, existing.origin, tpe.origin))
          case None           => (acc.updated(tpe.name, tpe), errs)
        }
      }
    }

  private def applyExtensions(
      types: Map[String, TypeDef],
      parsed: List[Parsed],
      synthesizableRoots: Set[String]
  ): (Map[String, TypeDef], Vector[SchemaError]) =
    parsed.flatMap(_.extensions).foldLeft((types, Vector.empty[SchemaError])) { case ((acc, errors), extension) =>
      val name = extension.tpe.name
      acc.get(name) match {
        case Some(existing) if existing.kind != extension.tpe.kind =>
          (
            acc,
            errors :+ SchemaError
              .KindMismatch(name, existing.origin, existing.kind, extension.tpe.kind, extension.origin)
          )
        case Some(existing)                                        => extendOne(acc, existing, extension, errors)
        case None if synthesizableRoots(name)                      =>
          val synthesized = TypeDef.empty(name, extension.tpe.kind, Origin.synthesized)
          extendOne(acc.updated(name, synthesized), synthesized, extension, errors)
        case None                                                  => (acc, errors :+ SchemaError.UnknownExtension(name, extension.origin))
      }
    }

  private def extendOne(
      types: Map[String, TypeDef],
      tpe: TypeDef,
      extension: Extension,
      errors: Vector[SchemaError]
  ): (Map[String, TypeDef], Vector[SchemaError]) = {
    val (withFields, fieldErrors) =
      extension.tpe.fields.foldLeft((tpe, errors)) { case ((current, errs), field) =>
        current.fields.find(_.name == field.name) match {
          case Some(existing) =>
            (current, errs :+ SchemaError.FieldCollision(tpe.name, field.name, existing.origin, field.origin))
          case None           => (current.copy(fields = current.fields :+ field.copy(fromExtension = true)), errs)
        }
      }

    val withEnums      = withFields.copy(enumValues = (withFields.enumValues ++ extension.tpe.enumValues).distinct)
    val withUnions     = withEnums.copy(unionMembers = (withEnums.unionMembers ++ extension.tpe.unionMembers).distinct)
    val withInterfaces =
      withUnions.copy(interfaces = (withUnions.interfaces ++ extension.tpe.interfaces).distinct)

    (types.updated(tpe.name, withInterfaces), fieldErrors)
  }

  private def resolveRoots(
      types: Map[String, TypeDef],
      explicit: Map[RootOperation, Root]
  ): (Map[RootOperation, String], Vector[SchemaError]) = {
    val resolved = scala.collection.mutable.Map.empty[RootOperation, String]
    val errors   = Vector.newBuilder[SchemaError]

    RootOperation.values.foreach { op =>
      explicit.get(op) match {
        case Some(root) if types.contains(root.tpe) => resolved.update(op, root.tpe)
        case Some(root)                             =>
          errors += SchemaError.RootNotDefined(op.label, root.tpe, root.origin)
        case None                                   =>
          val conventional = op match {
            case RootOperation.Query        => "Query"
            case RootOperation.Mutation     => "Mutation"
            case RootOperation.Subscription => "Subscription"
          }
          if (types.contains(conventional)) resolved.update(op, conventional)
      }
    }

    (resolved.toMap, errors.result())
  }
}
