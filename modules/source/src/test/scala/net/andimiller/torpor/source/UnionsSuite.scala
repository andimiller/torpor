package net.andimiller.torpor.source

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.{Encoder, Json}
import net.andimiller.torpor.macros.schema

final case class TaggedMember(kind: String, value: String)

/** Unions and interfaces: fragments on a member, `__typename` at every level, and a nested union. */
final class UnionsSuite extends munit.CatsEffectSuite {

  private val sdl = schema"""type Leaf { value: String! }
    |type And { values: [Expression!]! }
    |union Expression = Leaf | And
    |type Plain { id: ID! }
    |extend type Query { expression: Expression, plain: Plain }"""

  private val expression = Json.obj(
    "__typename" -> Json.fromString("And"),
    "values"     -> Json.arr(
      Json.obj("__typename" -> Json.fromString("Leaf"), "value" -> Json.fromString("a")),
      Json.obj("__typename" -> Json.fromString("Leaf"), "value" -> Json.fromString("b"))
    )
  )

  private val source = Source[IO]("expressions", sdl)(
    resolver.root[IO](sdl.Query.expression) { _ => IO.pure(expression) },
    resolver.root[IO](sdl.Query.plain) { _ => IO.pure(Json.obj("id" -> Json.fromString("p1"))) }
  )

  private val torpor = Torpor[IO](source)
  private val schema = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

  test("fragments select by __typename, nested two levels") {
    val query =
      """{ expression {
        |    __typename
        |    ... on And { values { __typename ... on Leaf { value } } }
        |    ... on Leaf { value }
        |  }
        |  plain { __typename id }
        |}""".stripMargin

    torpor.query(schema, query) match {
      case Left(error) => IO(fail(error.render.mkString("\n")))
      case Right(io)   =>
        io.map { response =>
          val data = response.data.hcursor
          assertEquals(data.downField("expression").get[String]("__typename").toOption, Some("And"))
          assertEquals(
            data.downField("expression").downField("values").downN(1).get[String]("value").toOption,
            Some("b")
          )
          assertEquals(
            data.downField("expression").downField("values").downN(0).get[String]("__typename").toOption,
            Some("Leaf")
          )
          assert(
            data.downField("expression").downField("value").focus.isEmpty,
            "the Leaf fragment contributes nothing to an And value"
          )
          assertEquals(data.downField("plain").get[String]("__typename").toOption, Some("Plain"))
          assertEquals(data.downField("plain").get[String]("id").toOption, Some("p1"))
          assertEquals(response.errors.toList, Nil)
        }
    }
  }

  test("a member-specific extension field becomes a node with the member's coordinate") {
    // `values` is declared by And itself, so it is read inline; this checks the wiring for a field added by another source.
    val extension = schema"""type Leaf { value: String! }
      |type And { values: [Expression!]! }
      |union Expression = Leaf | And
      |extend type And { count: Int! }
      |extend type Query { expression: Expression }"""
    val source    = Source[IO]("expressions", extension)(
      resolver.root[IO](extension.Query.expression) { _ => IO.pure(expression) },
      resolver
        .field[IO](extension.And.count)
        .key((and: Json) => and.hcursor.get[String]("__typename").getOrElse(""))
        .batch { (_, keys: NonEmptyList[String]) =>
          IO.pure(keys.toList.map(_ -> 2).toMap)
        }
    )
    val torpor    = Torpor[IO](source)
    val schema    = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

    torpor.query(schema, "{ expression { ... on And { count } } }") match {
      case Left(error) => IO(fail(error.render.mkString("\n")))
      case Right(io)   =>
        io.map(response =>
          assertEquals(response.data.hcursor.downField("expression").get[Int]("count").toOption, Some(2))
        )
    }
  }

  test("resolver.tagged puts __typename on a typed union member") {
    val base: Encoder[TaggedMember] = Encoder.AsObject.derived[TaggedMember]
    val tagged                      = resolver.tagged(base)(_.kind)
    val json                        = tagged(TaggedMember("Leaf", "a"))
    assertEquals(json.hcursor.get[String]("__typename").toOption, Some("Leaf"))
    assertEquals(json.hcursor.get[String]("value").toOption, Some("a"))
  }
}
