package net.andimiller.torpor.source

import cats.data.NonEmptyChain
import cats.data.Chain
import cats.effect.Concurrent
import io.circe.Json
import net.andimiller.torpor.core.{Executor, FieldCoordinate, Plan, Resolver, Response, Value}
import net.andimiller.torpor.schema.{Executable, Introspection, Schema, SdlFragment, ValidationError}

/** Everything that stopped a service from starting: merge errors, then coverage errors. */
final class TorporStartupException(val problems: List[String])
    extends RuntimeException(s"torpor cannot start:\n${problems.mkString("\n")}")

/** Why a registered field cannot be served at startup. */
enum CoverageError(val render: String) {
  case MissingResolver(source: String, coordinate: FieldCoordinate)
      extends CoverageError(
        s"no resolver for ${coordinate.render} in source '$source'; the field is declared by that source's SDL, so it must be served"
      )

  case UnexpectedResolver(source: String, coordinate: FieldCoordinate)
      extends CoverageError(
        s"resolver for ${coordinate.render} in source '$source' has no field to serve; it is either not in the SDL or not an extension field"
      )
}

/** A failed request, before or during planning. */
enum QueryError                        {
  case Validation(errors: NonEmptyChain[ValidationError])
  case Planning(errors: NonEmptyChain[PlanError])

  def render: List[String] =
    this match {
      case Validation(errors) => errors.toChain.toList.map(_.render)
      case Planning(errors)   => errors.toChain.toList.map(_.render)
    }
}

/** The facade: merge sources into a schema, check coverage, plan a request, run it.
  *
  * The registry is built once. `checkCoverage` is the startup gate: it reports every SDL field that has no resolver and
  * every resolver that has no field, before any request arrives.
  */
final class Torpor[F[_]: Concurrent] private (sources: List[Source[F, ?, ?]], limits: Planner.Limits) {

  private val registry: Map[(String, FieldCoordinate), Resolver[F]] =
    sources
      .flatMap(source => source.entries.map { case (coordinate, resolver) => (source.id -> coordinate) -> resolver })
      .toMap

  def withLimits(limits: Planner.Limits): Torpor[F] = new Torpor[F](sources, limits)

  def schema: Either[NonEmptyChain[net.andimiller.torpor.schema.SchemaError], Schema] =
    // a source may exist only to serve fields another source declares, so a blank fragment is not an error
    Schema.merge(sources.filter(_.sdl.nonEmpty).map(source => SdlFragment(source.id, source.sdl)))

  /** Merge and check coverage in one step, raising one exception with every problem. Servable construction goes through
    * here, so no route can be built from a schema whose unserved fields nobody checked.
    */
  def startup: F[(Torpor[F], Schema)] =
    schema match {
      case Left(errors)  => Concurrent[F].raiseError(new TorporStartupException(errors.toChain.toList.map(_.render)))
      case Right(merged) =>
        checkCoverage(merged) match {
          case Left(errors) => Concurrent[F].raiseError(new TorporStartupException(errors.toChain.toList.map(_.render)))
          case Right(())    => Concurrent[F].pure((this, merged))
        }
    }

  /** Every field a source declares via `extend type` needs a resolver, and so does every root field. Anything else is
    * returned by whatever produced the value, so a resolver for it is a mistake worth reporting.
    */
  def checkCoverage(schema: Schema): Either[NonEmptyChain[CoverageError], Unit] = {
    val extensionFields = schema.types.values.toList.flatMap { tpe =>
      tpe.fields.filter(_.fromExtension).map(field => field.origin.source -> FieldCoordinate(tpe.name, field.name))
    }
    val rootFields      =
      List(schema.query, schema.mutation, schema.subscription).flatten.flatMap(schema.types.get).flatMap { tpe =>
        tpe.fields.map(field => field.origin.source -> FieldCoordinate(tpe.name, field.name))
      }

    val required   = (extensionFields ++ rootFields).toSet
    val registered = registry.keySet

    val missing    = (required -- registered).toList.sortBy(_._2.render).map { case (source, coordinate) =>
      CoverageError.MissingResolver(source, coordinate)
    }
    val unexpected = (registered -- required).toList.sortBy(_._2.render).map { case (source, coordinate) =>
      CoverageError.UnexpectedResolver(source, coordinate)
    }

    NonEmptyChain.fromSeq(missing ++ unexpected).toLeft(())
  }

  private[source] def resolverFor(source: String, coordinate: FieldCoordinate): Option[Resolver[F]] =
    registry.get(source -> coordinate)

  /** A durable runtime: same sources, but suspended runs are persisted and can be resumed after a restart. */
  def durable(store: net.andimiller.torpor.snapshot.SnapshotStore[F]): F[Durable[F]]                =
    Durable[F](this, store)

  def run(plan: Plan): F[Response]   =
    new Executor[F](plan, (source, coordinate) => registry.get(source -> coordinate)).run

  /** The whole request path: validate, plan, run. */
  def query(
      schema: Schema,
      query: String,
      operationName: Option[String] = None,
      variables: Map[String, Value] = Map.empty
  ): Either[QueryError, F[Response]] =
    // Introspection is answered from the schema: no validation, no plan, no resolvers.
    Introspection.handle(schema, query) match {
      case Some(data) => Right(Concurrent[F].pure(Response(data, Chain.empty)))
      case None       => validated(schema, query, operationName, variables)
    }

  private def validated(
      schema: Schema,
      query: String,
      operationName: Option[String],
      variables: Map[String, Value]
  ): Either[QueryError, F[Response]] =
    Executable.validate(schema, query, operationName) match {
      case Left(errors)     => Left(QueryError.Validation(errors))
      case Right(validated) =>
        Planner.build(schema, validated, variables, limits) match {
          case Left(problems) => Left(QueryError.Planning(problems))
          case Right(plan)    => Right(run(plan))
        }
    }
}

object Torpor {

  /** One `(id, facts)` pair per source, which is what `LinkCheck` reads. */
  type SourcesOf[Ss] <: Tuple = Ss match {
    case Source[?, id, facts] => (id, facts) *: EmptyTuple
    case EmptyTuple           => EmptyTuple
    case s *: rest            => Tuple.Concat[SourcesOf[s], SourcesOf[rest]]
  }

  def apply[F[_]]: Start[F] = new Start[F]

  // inline methods cannot call a private constructor, so they come through here
  private def assemble[F[_]: Concurrent](sources: List[Source[F, ?, ?]]): Torpor[F] =
    new Torpor[F](sources, Planner.Limits.none)

  final class Start[F[_]] {

    /** `Torpor[IO](charactersSource, universeSource)`, in any order. Does not compile if a keyed resolver decodes its
      * parent as a type with a field that some producer of that parent type does not have.
      */
    inline def apply[Ss <: Matchable](sources: Ss)(using Concurrent[F]): Torpor[F] = {
      LinkCheck.verify[SourcesOf[Ss]]
      assemble[F](untyped(sources))
    }

    private def untyped(sources: Matchable): List[Source[F, ?, ?]] = sources match {
      case source: Source[F, ?, ?] @unchecked => List(source)
      case tuple: Tuple                       =>
        tuple.productIterator.toList.flatMap(element => untyped(element.asInstanceOf[Matchable]))
      case other                              => throw new IllegalArgumentException(s"not a source: $other")
    }
  }
}

object Variables {

  /** Client variables are JSON; the planner only substitutes them, so no coercion happens here. */
  def fromJson(json: Json): Map[String, Value] =
    json.asObject.map(_.toMap.map { case (name, value) => name -> Value.fromJson(value) }).getOrElse(Map.empty)
}
