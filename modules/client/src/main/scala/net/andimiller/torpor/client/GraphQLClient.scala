package net.andimiller.torpor.client

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.{Decoder, Json, JsonObject}
import org.http4s.{MediaType, Method, Status, Uri}
import org.http4s.circe.*
import org.http4s.client.{Client, UnexpectedStatus}
import org.http4s.headers.{Accept, MediaRangeAndQValue}

/** One error from a GraphQL response: its message, where in the response it happened, and its code if it has one. */
final case class GraphQLError(message: String, path: List[String], code: Option[String])

object GraphQLError {

  given Decoder[GraphQLError] = Decoder.instance { cursor =>
    for {
      message <- cursor.get[String]("message")
      path    <- cursor.get[Option[List[Json]]]("path")
      code    <- cursor.downField("extensions").get[Option[String]]("code")
    } yield GraphQLError(
      message,
      // a path mixes field names and list indices
      path.toList.flatten.map(segment => segment.asString.getOrElse(segment.noSpaces)),
      code
    )
  }
}

/** The server answered, but with errors. Any data it sent alongside them is not read. */
final class GraphQLFailure(val errors: List[GraphQLError])
    extends RuntimeException(errors.map(_.message).mkString("; "))

/** Runs typed operations against a GraphQL endpoint over HTTP.
  *
  * Headers, authentication and tracing belong to the `Client` this is given, as for any other HTTP service.
  */
final class GraphQLClient[F[_]: Concurrent](client: Client[F], endpoint: Uri) {

  /** Runs a typed operation that has been given its variables: `client.run(pets(first = Some(2)))`. */
  def run[D](request: Request[D]): F[D] =
    send(request.document, request.name, request.variables)(using request.decoder)

  private def send[D: Decoder](document: String, name: Option[String], variables: JsonObject): F[D] = {
    val body    = Json.obj(
      "query"         -> Json.fromString(document),
      "operationName" -> name.fold(Json.Null)(Json.fromString),
      "variables"     -> Json.fromJsonObject(variables)
    )
    val request = org.http4s.Request[F](Method.POST, endpoint).withEntity(body).putHeaders(GraphQLClient.accept)

    client.run(request).use { response =>
      response.as[Json].attempt.flatMap {
        case Right(json)                           => read(json, response.status)
        case Left(_) if !response.status.isSuccess => raise(UnexpectedStatus(response.status, Method.POST, endpoint))
        case Left(unreadable)                      => raise(unreadable)
      }
    }
  }

  /** A response whose body is JSON, whatever its status: a server that refuses an operation may say why with a 4xx, and
    * those errors are worth more than the status.
    */
  private def read[D: Decoder](json: Json, status: Status): F[D] =
    json.hcursor.get[Option[List[GraphQLError]]]("errors").map(_.toList.flatten) match {
      case Left(failure)                    => raise(failure)
      case Right(errors) if errors.nonEmpty => raise(new GraphQLFailure(errors))
      case Right(_) if !status.isSuccess    => raise(UnexpectedStatus(status, Method.POST, endpoint))
      case Right(_)                         => Concurrent[F].fromEither(json.hcursor.get[D]("data"))
    }

  private def raise[A](error: Throwable): F[A] = Concurrent[F].raiseError(error)
}

object GraphQLClient {

  /** What a GraphQL server may answer with: the media type GraphQL over HTTP names, and plain JSON. */
  private val accept: Accept = Accept(
    MediaRangeAndQValue(new MediaType("application", "graphql-response+json")),
    MediaRangeAndQValue(MediaType.application.json)
  )
}
