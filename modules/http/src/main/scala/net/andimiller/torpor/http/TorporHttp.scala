package net.andimiller.torpor.http

import cats.effect.Concurrent
import cats.syntax.all._
import io.circe.{Json, JsonObject}
import net.andimiller.torpor.core.Value
import net.andimiller.torpor.schema.Schema
import net.andimiller.torpor.source.Torpor
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl

/** GraphQL over HTTP, POST only (M0). Nothing bespoke: a GraphQL request is a JSON body, and GraphQL errors are 200s.
  *
  * SSE, WebSocket and the resume endpoint are M2, on the incremental-delivery wire format.
  */
object TorporHttp {

  def routes[F[_]: Concurrent](torpor: Torpor[F], schema: Schema): HttpRoutes[F] =
    routes(torpor, schema, (_: Request[F]) => Concurrent[F].unit)

  /** `setup` runs before the query, so a request can put its caller into an `IOLocal` that resolvers read. That is the
    * one blessed way to give resolvers a request context; nothing is threaded through `Torpor`.
    */
  def routes[F[_]: Concurrent](
      torpor: Torpor[F],
      schema: Schema,
      setup: Request[F] => F[Unit]
  ): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    def errorJson(messages: List[String]): Json =
      Json.obj(
        "errors" -> Json.fromValues(messages.map(message => Json.obj("message" -> Json.fromString(message))))
      )

    def respond(body: Json): F[Response[F]] = {
      val cursor = body.hcursor
      cursor.get[String]("query") match {
        case Left(_) =>
          BadRequest(errorJson(List("request body must be a JSON object with a 'query' string")))

        case Right(query) =>
          val operationName = cursor.get[String]("operationName").toOption
          val variables     = cursor
            .get[JsonObject]("variables")
            .toOption
            .map(_.toMap.map { case (name, value) => name -> Value.fromJson(value) })
            .getOrElse(Map.empty[String, Value])

          torpor.query(schema, query, operationName, variables) match {
            case Left(error) => Ok(errorJson(error.render)) // GraphQL errors are 200, per the spec
            case Right(io)   => io.flatMap(response => Ok(response.toJson))
          }
      }
    }

    HttpRoutes.of[F] { case req @ POST -> Root / "graphql" =>
      setup(req) *> req.as[Json].attempt.flatMap {
        case Left(_)     => BadRequest(errorJson(List("request body must be JSON")))
        case Right(body) => respond(body)
      }
    }
  }
}
