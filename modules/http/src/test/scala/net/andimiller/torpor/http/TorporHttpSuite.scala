package net.andimiller.torpor.http

import cats.effect.IO
import io.circe.Json
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.source.*
import org.http4s.{EntityDecoder, Method, Request, Status, Uri}
import org.http4s.circe._

final class TorporHttpSuite extends munit.CatsEffectSuite {

  private val sdl = schema"""extend type Query { hello(name: String): String }"""

  private val source = Source[IO]("greetings", sdl.text)(
    resolver.root[IO](sdl.Query.hello) { args =>
      IO(s"hello ${args.name.getOrElse("world")}")
    }
  )

  private val torpor = Torpor[IO](source)
  private val schema = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)
  private val app    = TorporHttp.routes(torpor, schema).orNotFound

  private def post(body: String): IO[(Status, Json)] = {
    implicit val json: EntityDecoder[IO, Json] = jsonDecoder[IO]
    app
      .run(Request[IO](Method.POST, Uri.unsafeFromString("/graphql")).withEntity(body))
      .flatMap(response => response.as[Json].map(json => (response.status, json)))
  }

  test("a valid query returns data") {
    post("""{"query":"{ hello(name: \"Ada\") }"}""").map { case (status, json) =>
      assertEquals(status, Status.Ok)
      assertEquals(json.hcursor.downField("data").get[String]("hello").toOption, Some("hello Ada"))
    }
  }

  test("variables are passed through to the planner") {
    post("""{"query":"query Q($n: String) { hello(name: $n) }","variables":{"n":"Grace"}}""").map {
      case (status, json) =>
        assertEquals(status, Status.Ok)
        assertEquals(json.hcursor.downField("data").get[String]("hello").toOption, Some("hello Grace"))
    }
  }

  test("operationName selects among several operations") {
    val body =
      """{"query":"query A { hello(name: \"A\") } query B { hello(name: \"B\") }","operationName":"B"}"""
    post(body).map { case (status, json) =>
      assertEquals(status, Status.Ok)
      assertEquals(json.hcursor.downField("data").get[String]("hello").toOption, Some("hello B"))
    }
  }

  test("a document with several operations and no operationName is a GraphQL error") {
    post("""{"query":"query A { hello } query B { hello }"}""").map { case (status, json) =>
      assertEquals(status, Status.Ok)
      assert(json.hcursor.downField("errors").focus.isDefined)
      assert(json.noSpaces.contains("operationName"))
    }
  }

  test("an invalid query is a 200 with GraphQL errors, not a 400") {
    post("""{"query":"{ nope }"}""").map { case (status, json) =>
      assertEquals(status, Status.Ok)
      assert(json.hcursor.downField("data").focus.isEmpty)
      assert(json.noSpaces.contains("5.3.1"), json.noSpaces)
    }
  }

  test("a missing limit is reported as a GraphQL error") {
    val limited                                = Torpor[IO](source).withLimits(net.andimiller.torpor.source.Planner.Limits(maxDepth = 0))
    val limitedApp                             = TorporHttp.routes(limited, schema).orNotFound
    implicit val json: EntityDecoder[IO, Json] = jsonDecoder[IO]
    limitedApp
      .run(Request[IO](Method.POST, Uri.unsafeFromString("/graphql")).withEntity("""{"query":"{ hello }"}"""))
      .flatMap(response => response.as[Json].map(response.status -> _))
      .map { case (status, body) =>
        assertEquals(status, Status.Ok)
        assert(body.noSpaces.contains("maxDepth"), body.noSpaces)
      }
  }

  test("a body that is not JSON is a 400") {
    post("not json").map { case (status, json) =>
      assertEquals(status, Status.BadRequest)
      assert(json.hcursor.downField("errors").focus.isDefined)
    }
  }

  test("a JSON body without a query is a 400") {
    post("""{"operationName":"A"}""").map { case (status, json) =>
      assertEquals(status, Status.BadRequest)
      assert(json.noSpaces.contains("query"))
    }
  }

  test("only POST /graphql is routed") {
    app.run(Request[IO](Method.GET, Uri.unsafeFromString("/graphql"))).map { response =>
      assertEquals(response.status, Status.NotFound)
    }
  }
}
