package net.andimiller.torpor.core

import cats.Applicative
import cats.effect.Resource
import cats.syntax.all._
import io.circe.Json

import scala.concurrent.duration.FiniteDuration

/** What a hook may return in place of a value or in place of a throwable's message. The executor owns the path. */
final case class HookError(message: String, code: String)

/** The extension points: one type, defaulted to no-ops, so an application picks its logging, tracing and auth backend
  * without the core depending on one.
  *
  * Hooks are code, not data: a resumed durable run uses the hooks of the `Torpor` it resumes through.
  */
abstract class Hooks[F[_]](using protected val F: Applicative[F]) { self =>

  /** A span around one plan node (one batch call), named by coordinate. */
  def nodeSpan(coordinate: FieldCoordinate, level: Int): Resource[F, Unit] = Resource.unit[F]

  /** After a node ran: timing and call counts, per coordinate. */
  def onNode(coordinate: FieldCoordinate, level: Int, elapsed: FiniteDuration): F[Unit] =
    F.unit

  /** After a request was planned and executed. */
  def onQuery(query: String, elapsed: FiniteDuration): F[Unit] = F.unit

  /** Runs once per node with every value it produced, keyed by encoded batch key (`""` for a root field). A `Left`
    * replaces that value with a null and a field error; a missing key keeps the value unchanged.
    */
  def authorize(
      coordinate: FieldCoordinate,
      args: List[Argument],
      values: Map[String, Json]
  ): F[Map[String, Either[HookError, Json]]] =
    F.pure(Map.empty)

  /** Every resolver failure passes through here before it becomes a field error. The default hides the message of
    * anything that is not a client's fault: a hostname in an exception is not the client's business.
    */
  def onError(coordinate: FieldCoordinate, throwable: Throwable): F[HookError] =
    F.pure(
      throwable match {
        case e: InvalidInputException =>
          HookError(Option(e.getMessage).getOrElse("invalid input"), FieldError.InvalidInput)
        case _                        =>
          HookError("internal error", FieldError.UpstreamUnavailable)
      }
    )

  def compose(other: Hooks[F]): Hooks[F] = new Hooks[F] {
    override def nodeSpan(coordinate: FieldCoordinate, level: Int): Resource[F, Unit]              =
      self.nodeSpan(coordinate, level) >> other.nodeSpan(coordinate, level)
    override def onNode(coordinate: FieldCoordinate, level: Int, elapsed: FiniteDuration): F[Unit] =
      self.onNode(coordinate, level, elapsed) *> other.onNode(coordinate, level, elapsed)
    override def onQuery(query: String, elapsed: FiniteDuration): F[Unit]                          =
      self.onQuery(query, elapsed) *> other.onQuery(query, elapsed)
    override def authorize(
        coordinate: FieldCoordinate,
        args: List[Argument],
        values: Map[String, Json]
    ): F[Map[String, Either[HookError, Json]]]                                                     =
      (self.authorize(coordinate, args, values), other.authorize(coordinate, args, values))
        .mapN((first, second) => second ++ first)
    override def onError(coordinate: FieldCoordinate, throwable: Throwable): F[HookError]          =
      self.onError(coordinate, throwable)
  }
}

object Hooks {
  def noop[F[_]](using Applicative[F]): Hooks[F] = new Hooks[F] {}
}
