package net.andimiller.torpor.source

import cats.{Applicative, Functor}
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json, parser}
import net.andimiller.torpor.core.{Argument, FieldCoordinate, InvalidInputException, Resolver, Step}

/** A source: an SDL fragment plus the resolvers that serve it. */
final case class Source[F[_]](id: String, sdl: String, resolvers: Resolvers[F])

/** Registered resolvers, keyed by the field coordinate the merged schema gives them. */
final case class Resolvers[F[_]](entries: List[(FieldCoordinate, Resolver[F])]) {
  def ++(other: Resolvers[F]): Resolvers[F] = Resolvers(entries ++ other.entries)
}

object Resolvers {
  def apply[F[_]](entries: (FieldCoordinate, Resolver[F])*): Resolvers[F] = Resolvers(entries.toList)
  def empty[F[_]]: Resolvers[F]                                           = Resolvers(Nil)
}

/** `"Pet" / "owner"` reads better than `FieldCoordinate("Pet", "owner")` at call sites. */
extension (typeName: String) def /(fieldName: String): FieldCoordinate = FieldCoordinate(typeName, fieldName)

object resolver {

  def root[F[_]](coordinate: FieldCoordinate): RootBuilder[F] = new RootBuilder[F](coordinate)

  /** A root field that may park the run. The handle is encoded into the snapshot and decoded back on resume; the
    * webhook payload stays raw JSON, because it arrives from outside.
    */
  def suspending[F[_], A, H, B](coordinate: FieldCoordinate)(
      start: A => F[Step[B, H]]
  )(
      resume: (H, io.circe.Json) => F[Step[B, H]]
  )(using Functor[F], Decoder[A], Encoder[H], Decoder[H], Encoder[B]): (FieldCoordinate, Resolver[F]) =
    coordinate -> Resolver.Suspending(
      start = arguments =>
        val json = Json.obj(arguments.map(a => a.name -> a.value.toJson): _*)
        Decoder[A].decodeJson(json) match {
          case Left(error)    =>
            throw new InvalidInputException(s"cannot decode arguments for ${coordinate.render}: $error")
          case Right(decoded) =>
            start(decoded).map {
              case Step.Complete(value) => Step.Complete(Encoder[B].apply(value))
              case Step.Suspend(handle) => Step.Suspend(Encoder[H].apply(handle))
            }
        },
      resume = (handle, payload) =>
        parser.parse(handle.noSpaces).flatMap(Decoder[H].decodeJson) match {
          case Left(error)    => throw new InvalidInputException(s"cannot decode handle for ${coordinate.render}: $error")
          case Right(decoded) =>
            resume(decoded, payload).map {
              case Step.Complete(value) => Step.Complete(Encoder[B].apply(value))
              case Step.Suspend(next)   => Step.Suspend(Encoder[H].apply(next))
            }
        }
    )

  def field[F[_]](coordinate: FieldCoordinate): FieldBuilder[F] = new FieldBuilder[F](coordinate)

  /** A keyed field whose key may be absent (a nullable foreign key). Absent keys are never sent to `batch`; they
    * resolve to `null`, which is what "no value" means for a nullable field, and is not an error.
    */
  def optionalField[F[_]](coordinate: FieldCoordinate): OptionalFieldBuilder[F] =
    new OptionalFieldBuilder[F](coordinate)

  /** Not a valid JSON key encoding, so it cannot collide with a real key: encoded keys start with a JSON token. */
  private val AbsentKey = "\u0000absent"

  final class RootBuilder[F[_]](coordinate: FieldCoordinate) {
    def apply[A, B](run: A => F[B])(using Functor[F], Decoder[A], Encoder[B]): (FieldCoordinate, Resolver[F]) =
      coordinate -> Resolver.Root { arguments =>
        val json = Json.obj(arguments.map(a => a.name -> a.value.toJson): _*)
        Decoder[A].decodeJson(json) match {
          case Left(error)    =>
            throw new InvalidInputException(s"cannot decode arguments for ${coordinate.render}: $error")
          case Right(decoded) => run(decoded).map(Encoder[B].apply)
        }
      }
  }

  final class FieldBuilder[F[_]](coordinate: FieldCoordinate) {
    def key[P, K](extract: P => K): KeyedField[F, P, K] = new KeyedField[F, P, K](coordinate, extract)
  }

  /** The batch signature is `NonEmptyList[K] => F[Map[K, V]]`: there is deliberately no single-key overload, because
    * its absence is what makes N+1 impossible to write by accident.
    */
  final class KeyedField[F[_], P, K](coordinate: FieldCoordinate, extract: P => K) {
    def batch[V](run: NonEmptyList[K] => F[Map[K, V]])(using
        Applicative[F],
        Decoder[P],
        Encoder[K],
        Decoder[K],
        Encoder[V]
    ): (FieldCoordinate, Resolver[F]) =
      coordinate -> Resolver.Batch(
        key = value =>
          Decoder[P].decodeJson(value) match {
            case Left(error) => Left(s"cannot decode parent value for ${coordinate.render}: $error")
            case Right(p)    => Right(keyString(extract(p)))
          },
        batch = keys => run(keys.map(decodeKey)).map(_.map { case (k, v) => keyString(k) -> Encoder[V].apply(v) })
      )

    private def keyString(key: K)(using Encoder[K]): String = Encoder[K].apply(key).noSpaces

    private def decodeKey(encoded: String)(using Decoder[K]): K =
      decoderFor(encoded)
        .getOrElse(throw new InvalidInputException(s"cannot decode batch key '$encoded' for ${coordinate.render}"))
  }

  private def decoderFor[K](encoded: String)(using Decoder[K]): Option[K] =
    parser.parse(encoded).toOption.flatMap(Decoder[K].decodeJson(_).toOption)

  final class OptionalFieldBuilder[F[_]](coordinate: FieldCoordinate) {
    def key[P, K](extract: P => Option[K]): OptionalKeyedField[F, P, K] =
      new OptionalKeyedField[F, P, K](coordinate, extract)
  }

  final class OptionalKeyedField[F[_], P, K](coordinate: FieldCoordinate, extract: P => Option[K]) {
    def batch[V](run: NonEmptyList[K] => F[Map[K, V]])(using
        Applicative[F],
        Decoder[P],
        Encoder[K],
        Decoder[K],
        Encoder[V]
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
              run(nel.map(decodeKey))
                .map(results =>
                  results.map { case (k, v) => keyString(k) -> Encoder[V].apply(v) } + (AbsentKey -> Json.Null)
                )
          }
        }
      )

    private def keyString(key: K)(using Encoder[K]): String = Encoder[K].apply(key).noSpaces

    private def decodeKey(encoded: String)(using Decoder[K]): K =
      decoderFor(encoded).getOrElse(
        throw new InvalidInputException(s"cannot decode batch key '$encoded' for ${coordinate.render}")
      )
  }
}
