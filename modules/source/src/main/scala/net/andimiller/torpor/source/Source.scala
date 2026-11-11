package net.andimiller.torpor.source

import cats.{Applicative, Functor}
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json, parser}
import net.andimiller.torpor.core.{Arguments, FieldCoordinate, InvalidInputException, Resolver, Step}

/** A source: an SDL fragment plus the resolvers that serve it.
  *
  * Its type keeps what the resolvers erase: `Facts` records, for each one, which GraphQL type it produces as which
  * Scala type, and which Scala type a keyed field decodes its parent as. `Torpor[F](…)` reads them to check, when the
  * code compiles, that every keyed field can decode what each producer of its parent type produces.
  *
  * {{{
  * Source[IO]("universe", universe.text)(
  *   resolver.field[IO](universe.Character.corporation).key(…).batch(…),
  *   resolver.optionalField[IO](universe.Corporation.alliance).key(…).batch(…)
  * )
  * }}}
  */
final class Source[F[_], Id <: String, Facts <: Tuple] private (
    val id: Id,
    val sdl: String,
    private[source] val entries: List[(FieldCoordinate, Resolver[F])]
) {

  /** The resolvers that serve this source, as one argument list, given once. Anything that is not a resolver from the
    * `resolver` DSL does not compile.
    */
  def apply[Rs <: Matchable](resolvers: Rs)(using
      Facts =:= EmptyTuple
  ): Source[F, Id, Source.FactsOf[Rs]] =
    new Source(id, sdl, Source.entries(resolvers))
}

object Source {

  /** The facts of one resolver, or of a tuple of them, flattened into one tuple. */
  type FactsOf[Rs] <: Tuple = Rs match {
    case FieldResolver[?, facts] => facts
    case EmptyTuple              => EmptyTuple
    case r *: rest               => Tuple.Concat[FactsOf[r], FactsOf[rest]]
  }

  def apply[F[_]]: Start[F] = new Start[F]

  final class Start[F[_]] {

    /** A source with no resolvers yet; apply it to its resolvers. One that only declares types needs none. */
    def apply[Id <: String & Singleton](id: Id, sdl: String): Source[F, Id, EmptyTuple] = new Source(id, sdl, Nil)
  }

  private def entries[F[_]](resolvers: Matchable): List[(FieldCoordinate, Resolver[F])] = resolvers match {
    case resolver: FieldResolver[F, ?] @unchecked => List(resolver.entry)
    case tuple: Tuple                             =>
      tuple.productIterator.toList.flatMap(element => entries[F](element.asInstanceOf[Matchable]))
    case other                                    => throw new IllegalArgumentException(s"not a resolver: $other")
  }
}

/** A type-level fact about one resolver. Phantom: never instantiated. */
sealed trait Fact

/** The resolver at `Parent.Field` produces values of GraphQL type `G`, as Scala type `B`. `B` is the resolver's own
  * result type, so it may still be wrapped in `Option` or `List`; the check unwraps it.
  */
sealed abstract class Produces[G <: String, B, Parent <: String, Field <: String] extends Fact

/** The keyed resolver at `Parent.Field` decodes each parent, a value of GraphQL type `Parent`, as Scala type `P`. */
sealed abstract class Consumes[P, Parent <: String, Field <: String] extends Fact

/** One resolver, erased to JSON for the executor, plus the facts it contributes to its source's type. */
final class FieldResolver[F[_], Facts <: Tuple] private (private[source] val entry: (FieldCoordinate, Resolver[F]))

object FieldResolver {
  // inline methods cannot call a private constructor, so they come through here
  private[source] def apply[F[_], Facts <: Tuple](entry: (FieldCoordinate, Resolver[F])): FieldResolver[F, Facts] =
    new FieldResolver(entry)
}

// wiring code imports `source.*`; these are what it declares and reads
export net.andimiller.torpor.core.{Arguments, Scalar}

/** Resolvers take coordinates from the schema macro (`schema"""…"""`), whose types carry each field's parent, name,
  * arguments and GraphQL return type.
  */
object resolver {

  /** A root field. The lambda's `args` is typed from the SDL's arguments: `resolver.root[IO](sdl.Query.character) {
    * args => args.id … }`; `.args[A]` decodes them as `A` instead.
    */
  def root[F[_]]: RootStart[F] = new RootStart[F]

  final class RootStart[F[_]] {
    def apply[T <: String, N <: String, A, R <: String](
        coordinate: FieldCoordinate.Of[T, N, A, R]
    ): Root[F, T, N, A, R] =
      new Root(coordinate)
  }

  final class Root[F[_], T <: String, N <: String, A, R <: String](coordinate: FieldCoordinate) {
    inline def apply[B](run: A => F[B])(using
        functor: Functor[F],
        encoder: Encoder[B]
    ): FieldResolver[F, Produces[R, B, T, N] *: EmptyTuple] =
      FieldResolver(rootEntry[F, A, B](coordinate, run)(using functor, Arguments.decoder[A], encoder))

    /** Decode the arguments as `A2` instead, such as a case class with domain types the SDL cannot express. */
    def args[A2]: Root.Decoding[F, T, N, A2, R] = new Root.Decoding(coordinate)
  }

  object Root {
    final class Decoding[F[_], T <: String, N <: String, A, R <: String](coordinate: FieldCoordinate) {
      def apply[B](run: A => F[B])(using
          Functor[F],
          Decoder[A],
          Encoder[B]
      ): FieldResolver[F, Produces[R, B, T, N] *: EmptyTuple] =
        FieldResolver(rootEntry[F, A, B](coordinate, run))
    }
  }

  /** A root field that may park the run. The handle is encoded into the snapshot and decoded back on resume; the
    * webhook payload stays raw JSON, because it arrives from outside.
    */
  def suspending[F[_]]: SuspendingStart[F] = new SuspendingStart[F]

  final class SuspendingStart[F[_]] {
    def apply[T <: String, N <: String, A, R <: String](
        coordinate: FieldCoordinate.Of[T, N, A, R]
    ): Suspending[F, T, N, A, R] =
      new Suspending(coordinate)
  }

  final class Suspending[F[_], T <: String, N <: String, A, R <: String](coordinate: FieldCoordinate) {
    inline def apply[H, B](start: A => F[Step[B, H]])(resume: (H, Json) => F[Step[B, H]])(using
        functor: Functor[F],
        encodeHandle: Encoder[H],
        decodeHandle: Decoder[H],
        encoder: Encoder[B]
    ): FieldResolver[F, Produces[R, B, T, N] *: EmptyTuple] =
      FieldResolver(
        suspendingEntry[F, A, H, B](coordinate, start, resume)(using
          functor,
          Arguments.decoder[A],
          encodeHandle,
          decodeHandle,
          encoder
        )
      )
  }

  def field[F[_]]: FieldStart[F] = new FieldStart[F]

  final class FieldStart[F[_]] {
    def apply[T <: String, N <: String, A, R <: String](coordinate: FieldCoordinate.Of[T, N, A, R]): Field[F, T, N, R] =
      new Field(coordinate)
  }

  final class Field[F[_], T <: String, N <: String, R <: String](coordinate: FieldCoordinate) {
    def key[P, K](extract: P => K): KeyedField[F, T, N, R, P, K] = new KeyedField(coordinate, extract)
  }

  /** A keyed field both consumes its parent and produces its own values, which are the next level's parents.
    *
    * The batch signature is `NonEmptyList[K] => F[Map[K, V]]`: there is deliberately no single-key overload, because
    * its absence is what makes N+1 impossible to write by accident.
    */
  final class KeyedField[F[_], T <: String, N <: String, R <: String, P, K](
      coordinate: FieldCoordinate,
      extract: P => K
  ) {
    def batch[V](run: NonEmptyList[K] => F[Map[K, V]])(using
        Applicative[F],
        Decoder[P],
        Encoder[K],
        Decoder[K],
        Encoder[V]
    ): FieldResolver[F, (Consumes[P, T, N], Produces[R, V, T, N])] =
      FieldResolver(keyedEntry(coordinate, extract, run))
  }

  /** A keyed field whose key may be absent (a nullable foreign key). Absent keys are never sent to `batch`; they
    * resolve to `null`, which is what "no value" means for a nullable field, and is not an error.
    */
  def optionalField[F[_]]: OptionalFieldStart[F] = new OptionalFieldStart[F]

  final class OptionalFieldStart[F[_]] {
    def apply[T <: String, N <: String, A, R <: String](
        coordinate: FieldCoordinate.Of[T, N, A, R]
    ): OptionalField[F, T, N, R] =
      new OptionalField(coordinate)
  }

  final class OptionalField[F[_], T <: String, N <: String, R <: String](coordinate: FieldCoordinate) {
    def key[P, K](extract: P => Option[K]): OptionalKeyedField[F, T, N, R, P, K] =
      new OptionalKeyedField(coordinate, extract)
  }

  final class OptionalKeyedField[F[_], T <: String, N <: String, R <: String, P, K](
      coordinate: FieldCoordinate,
      extract: P => Option[K]
  ) {
    def batch[V](run: NonEmptyList[K] => F[Map[K, V]])(using
        Applicative[F],
        Decoder[P],
        Encoder[K],
        Decoder[K],
        Encoder[V]
    ): FieldResolver[F, (Consumes[P, T, N], Produces[R, V, T, N])] =
      FieldResolver(optionalKeyedEntry(coordinate, extract, run))
  }

  // the erased resolvers the executor runs

  /** Not a valid JSON key encoding, so it cannot collide with a real key: encoded keys start with a JSON token. */
  private val AbsentKey = "\u0000absent"

  private def argumentsJson(arguments: List[net.andimiller.torpor.core.Argument]): Json =
    Json.obj(arguments.map(a => a.name -> a.value.toJson): _*)

  private def rootEntry[F[_]: Functor, A: Decoder, B: Encoder](
      coordinate: FieldCoordinate,
      run: A => F[B]
  ): (FieldCoordinate, Resolver[F])                                                     =
    coordinate -> Resolver.Root { arguments =>
      Decoder[A].decodeJson(argumentsJson(arguments)) match {
        case Left(error)    => throw new InvalidInputException(s"cannot decode arguments for ${coordinate.render}: $error")
        case Right(decoded) => run(decoded).map(Encoder[B].apply)
      }
    }

  private def suspendingEntry[F[_]: Functor, A: Decoder, H: Encoder: Decoder, B: Encoder](
      coordinate: FieldCoordinate,
      start: A => F[Step[B, H]],
      resume: (H, Json) => F[Step[B, H]]
  ): (FieldCoordinate, Resolver[F]) = {
    def encoded(step: F[Step[B, H]]): F[Step[Json, Json]] = step.map {
      case Step.Complete(value) => Step.Complete(Encoder[B].apply(value))
      case Step.Suspend(handle) => Step.Suspend(Encoder[H].apply(handle))
    }

    coordinate -> Resolver.Suspending(
      start = arguments =>
        Decoder[A].decodeJson(argumentsJson(arguments)) match {
          case Left(error)    =>
            throw new InvalidInputException(s"cannot decode arguments for ${coordinate.render}: $error")
          case Right(decoded) => encoded(start(decoded))
        },
      resume = (handle, payload) =>
        parser.parse(handle.noSpaces).flatMap(Decoder[H].decodeJson) match {
          case Left(error)    => throw new InvalidInputException(s"cannot decode handle for ${coordinate.render}: $error")
          case Right(decoded) => encoded(resume(decoded, payload))
        }
    )
  }

  private def keyedEntry[F[_]: Applicative, P: Decoder, K: Encoder: Decoder, V: Encoder](
      coordinate: FieldCoordinate,
      extract: P => K,
      run: NonEmptyList[K] => F[Map[K, V]]
  ): (FieldCoordinate, Resolver[F]) =
    coordinate -> Resolver.Batch(
      key = value =>
        Decoder[P].decodeJson(value) match {
          case Left(error) => Left(s"cannot decode parent value for ${coordinate.render}: $error")
          case Right(p)    => Right(keyString(extract(p)))
        },
      batch = keys =>
        run(keys.map(decodeKey[K](coordinate, _))).map(_.map { case (k, v) => keyString(k) -> Encoder[V].apply(v) })
    )

  private def optionalKeyedEntry[F[_]: Applicative, P: Decoder, K: Encoder: Decoder, V: Encoder](
      coordinate: FieldCoordinate,
      extract: P => Option[K],
      run: NonEmptyList[K] => F[Map[K, V]]
  ): (FieldCoordinate, Resolver[F]) =
    coordinate -> Resolver.Batch(
      key = value =>
        Decoder[P].decodeJson(value) match {
          case Left(error) => Left(s"cannot decode parent value for ${coordinate.render}: $error")
          case Right(p)    => Right(extract(p).fold(AbsentKey)(keyString(_)))
        },
      batch = keys => {
        val present = keys.toList.filterNot(_ == AbsentKey).distinct
        NonEmptyList.fromList(present) match {
          case None      => Applicative[F].pure(Map(AbsentKey -> Json.Null))
          case Some(nel) =>
            run(nel.map(decodeKey[K](coordinate, _)))
              .map(results =>
                results.map { case (k, v) => keyString(k) -> Encoder[V].apply(v) } + (AbsentKey -> Json.Null)
              )
        }
      }
    )

  private def keyString[K: Encoder](key: K): String = Encoder[K].apply(key).noSpaces

  private def decodeKey[K: Decoder](coordinate: FieldCoordinate, encoded: String): K =
    parser
      .parse(encoded)
      .toOption
      .flatMap(Decoder[K].decodeJson(_).toOption)
      .getOrElse(throw new InvalidInputException(s"cannot decode batch key '$encoded' for ${coordinate.render}"))
}
