package net.andimiller.torpor.examples.esi

import cats.data.NonEmptyChain
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

  private val badBody = "body must be JSON with a 'query' string\n"

  private def renderProblems[E](problems: NonEmptyChain[E])(render: E => String): String =
    problems.toChain.toList.map(render).mkString("\n") + "\n"

  def routes(schema: Schema): HttpRoutes[IO] = HttpRoutes.of[IO] { case req @ POST -> Root / "explain" =>
    req.as[Json].attempt.flatMap {
      case Left(_)     => BadRequest(badBody)
      case Right(body) =>
        val explained = for {
          query     <- body.hcursor.get[String]("query").left.map(_ => badBody)
          validated <- Executable
                         .validate(schema, query, body.hcursor.get[String]("operationName").toOption)
                         .left
                         .map(renderProblems(_)(_.render))
          plan      <- Planner.build(schema, validated).left.map(renderProblems(_)(_.render))
        } yield Explain.mermaid(plan) + "\n\n" + Explain.report(plan) + "\n"
        Ok(explained.merge)
    }
  }
}
