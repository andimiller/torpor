package net.andimiller.torpor.source

import cats.effect.IO
import io.circe.{Decoder, Encoder, Json}
import net.andimiller.torpor.core.{FieldError, PathSegment, Scalar}
import net.andimiller.torpor.macros.schema

import java.time.Instant
import scala.util.Try

final case class FlagFilter(key: String, value: Boolean) derives Decoder
final case class OrganizationFilters(hasFlags: Option[List[FlagFilter]] = None, limit: Option[Int] = None)
    derives Decoder
final case class OrganizationsArgs(filters: Option[OrganizationFilters]) derives Decoder

/** Input coercion: input objects, defaults, the built-in scalars, and a custom scalar failing with `INVALID_INPUT`. */
final class InputObjectsSuite extends munit.CatsEffectSuite {

  given Decoder[Instant]           =
    Decoder.decodeString.emap(s => Try(Instant.parse(s)).toEither.left.map(_ => s"not an ISO instant: $s"))
  given Encoder[Instant]           = Encoder.encodeString.contramap(_.toString)
  given Scalar["Instant", Instant] = Scalar()

  private val sdl = schema"""scalar Instant
    |scalar Json
    |input FlagFilter { key: String!, value: Boolean! }
    |input OrganizationFilters { hasFlags: [FlagFilter!], limit: Int = 5 }
    |type Organization { id: ID! config: Json createdAt: Instant }
    |extend type Query {
    |  organizations(filters: OrganizationFilters): [Organization!]!
    |  at(instant: Instant): String
    |}"""

  private val source = Source[IO]("organizations", sdl)(
    resolver.root[IO](sdl.Query.organizations).args[OrganizationsArgs] { args =>
      val filters = args.filters
      IO.pure(
        Json.arr(
          Json.obj(
            "id"        -> Json.fromString(s"limit-${filters.flatMap(_.limit).getOrElse(-1)}"),
            "config"    -> Json.obj("nested" -> Json.obj("x" -> Json.fromInt(1))),
            "createdAt" -> Json.fromString("2024-01-01T00:00:00Z")
          )
        )
      )
    },
    resolver.root[IO](sdl.Query.at) { args =>
      IO.pure(args.instant.map(_.toString).getOrElse("none"))
    }
  )

  private val torpor = Torpor[IO](source)
  private val schema = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

  private def execute(query: String) =
    torpor.query(schema, query) match {
      case Left(error) => IO(fail(error.render.mkString("\n")))
      case Right(io)   => io
    }

  test("a nested input object is typed, defaults are filled, and a Json scalar passes through") {
    val query =
      """{ organizations(filters: { hasFlags: [{ key: "a", value: true }] }) { id config } }"""
    execute(query).map { response =>
      val organization = response.data.hcursor.downField("organizations").downArray
      assertEquals(organization.get[String]("id").toOption, Some("limit-5"), "the SDL default was filled in")
      assertEquals(
        organization.downField("config").focus,
        Some(Json.obj("nested" -> Json.obj("x" -> Json.fromInt(1))))
      )
      assertEquals(response.errors.toList, Nil)
    }
  }

  test("an unknown input field is a request error") {
    torpor.query(schema, """{ organizations(filters: { nope: 1 }) { id } }""") match {
      case Left(error) => assert(error.render.exists(_.contains("has no field 'nope'")), error.render.mkString("\n"))
      case Right(_)    => fail("expected a planning error")
    }
  }

  test("a missing non-null field inside an input object is a request error") {
    torpor.query(schema, """{ organizations(filters: { hasFlags: [{ key: "a" }] }) { id } }""") match {
      case Left(error) => assert(error.render.exists(_.contains("requires field 'value'")), error.render.mkString("\n"))
      case Right(_)    => fail("expected a planning error")
    }
  }

  test("an Int outside 32 bits is a request error") {
    torpor.query(schema, """{ organizations(filters: { limit: 99999999999 }) { id } }""") match {
      case Left(error) => assert(error.render.exists(_.contains("32 bits")), error.render.mkString("\n"))
      case Right(_)    => fail("expected a planning error")
    }
  }

  test("a custom scalar its decoder rejects is INVALID_INPUT at the field path") {
    execute("""{ at(instant: "not-a-time") }""").map { response =>
      assert(response.data.hcursor.downField("at").focus.contains(Json.Null))
      val errors = response.errors.toList
      assertEquals(errors.map(_.code), List(FieldError.InvalidInput))
      assertEquals(errors.head.path, List(PathSegment.Field("at")))
      assert(errors.head.message.contains("not an ISO instant"), errors.head.message)
    }
  }

  test("a valid custom scalar reaches the resolver as its Scala type") {
    execute("""{ at(instant: "2024-01-01T00:00:00Z") }""").map { response =>
      assertEquals(
        response.data.hcursor.get[String]("at").toOption,
        Some("2024-01-01T00:00:00Z")
      )
    }
  }
}
