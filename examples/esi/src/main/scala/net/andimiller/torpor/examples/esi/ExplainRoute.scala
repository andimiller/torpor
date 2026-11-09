package net.andimiller.torpor.examples.esi

import cats.effect.IO
import io.circe.Json
import net.andimiller.torpor.explain.Explain
import net.andimiller.torpor.schema.{Executable, Schema}
import net.andimiller.torpor.source.Planner
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*

/** Dev-only: validate and plan the incoming query, then print the plan. No resolver runs, so this is the cost of the
  * request before paying it, and it is the same `Plan` value the executor would run.
  *
  * Variables are not substituted here: this route takes literal queries only.
  */
object ExplainRoute {

  def routes(schema: Schema): HttpRoutes[IO] = HttpRoutes.of[IO] { case req @ POST -> Root / "explain" =>
    req.as[Json].attempt.flatMap {
      case Left(_)     => BadRequest("body must be JSON with a 'query' string\n")
      case Right(body) =>
        body.hcursor.get[String]("query") match {
          case Left(_)      => BadRequest("body must be JSON with a 'query' string\n")
          case Right(query) =>
            Executable.validate(schema, query, body.hcursor.get[String]("operationName").toOption) match {
              case Left(errors)     => Ok(errors.toChain.toList.map(_.render).mkString("\n") + "\n")
              case Right(validated) =>
                Planner.build(schema, validated) match {
                  case Left(problems) => Ok(problems.toChain.toList.map(_.render).mkString("\n") + "\n")
                  case Right(plan)    => Ok(Explain.mermaid(plan) + "\n\n" + Explain.report(plan) + "\n")
                }
            }
        }
    }
  }
}
