package net.andimiller.torpor.source

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Ref
import io.circe.{Decoder, Encoder, Json}
import net.andimiller.torpor.core.{FieldCoordinate, FieldError, HookError, Hooks, PathSegment}
import net.andimiller.torpor.macros.schema

import scala.concurrent.duration.FiniteDuration

final case class GuardedItem(id: String) derives Encoder.AsObject, Decoder
final case class GuardedDetail(id: String, secret: String) derives Encoder.AsObject, Decoder

/** One hook interface covers authorization, error mapping, call counts and slow-query logging. */
final class HooksSuite extends munit.CatsEffectSuite {

  private val sdl = schema"""type GuardedItem { id: ID! }
    |type GuardedDetail { id: ID! secret: String! }
    |extend type GuardedItem { detail: GuardedDetail }
    |extend type Query { items: [GuardedItem!]! }"""

  private def fixture(hooks: Hooks[IO]) =
    Source[IO]("guarded", sdl)(
      resolver.root[IO](sdl.Query.items) { _ =>
        IO.pure(List(GuardedItem("a"), GuardedItem("b")))
      },
      resolver
        .field[IO](sdl.GuardedItem.detail)
        .key((item: GuardedItem) => item.id)
        .batch { (_, keys: NonEmptyList[String]) =>
          IO.pure(keys.toList.map(id => id -> GuardedDetail(s"$id-detail", "secret")).toMap)
        }
    )

  test("authorize replaces a batch value with a FORBIDDEN error at that path") {
    val hooks = new Hooks[IO] {
      override def authorize(
          coordinate: FieldCoordinate,
          args: List[net.andimiller.torpor.core.Argument],
          values: Map[String, Json]
      ) =
        IO.pure(
          values.map { case (key, value) =>
            key -> (if (key == "\"b\"") Left(HookError("not allowed", FieldError.Forbidden)) else Right(value))
          }
        )
    }

    val source = fixture(hooks)
    val torpor = Torpor[IO](source).withHooks(hooks)
    val schema = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

    torpor.query(schema, "{ items { id detail { id } } }") match {
      case Left(error) => IO(fail(error.render.mkString("\n")))
      case Right(io)   =>
        io.map { response =>
          val items = response.data.hcursor.downField("items")
          assertEquals(items.downN(0).downField("detail").get[String]("id").toOption, Some("a-detail"))
          assert(items.downN(1).downField("detail").focus.contains(Json.Null))

          val errors = response.errors.toList
          assertEquals(errors.size, 1)
          assertEquals(errors.head.code, FieldError.Forbidden)
          assertEquals(
            errors.head.path,
            List(PathSegment.Field("items"), PathSegment.Index(1), PathSegment.Field("detail"))
          )
        }
    }
  }

  test("onError sees the throwable and can choose the client's message") {
    val sdl    = schema"""extend type Query { boom: String }"""
    val source = Source[IO]("boom", sdl)(
      resolver.root[IO](sdl.Query.boom) { _ => IO.raiseError[String](new RuntimeException("db password is hunter2")) }
    )
    val torpor = Torpor[IO](source).withHooks(new Hooks[IO] {
      override def onError(coordinate: FieldCoordinate, throwable: Throwable) =
        IO.pure(HookError(s"${coordinate.render} failed: ${throwable.getMessage}", "CUSTOM"))
    })
    val schema = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

    torpor.query(schema, "{ boom }") match {
      case Left(error) => IO(fail(error.render.mkString("\n")))
      case Right(io)   =>
        io.map { response =>
          assertEquals(response.errors.toList.map(_.code), List("CUSTOM"))
          assertEquals(response.errors.toList.map(_.message), List("Query.boom failed: db password is hunter2"))
        }
    }
  }

  test("onNode and onQuery report every node and one query") {
    for {
      nodes <- Ref.of[IO, List[FieldCoordinate]](Nil)
      query <- Ref.of[IO, Int](0)
      hooks  = new Hooks[IO] {
                 override def onNode(coordinate: FieldCoordinate, level: Int, elapsed: FiniteDuration) =
                   nodes.update(_ :+ coordinate)
                 override def onQuery(text: String, elapsed: FiniteDuration)                           = query.update(_ + 1)
               }
      torpor = Torpor[IO](fixture(hooks)).withHooks(hooks)
      schema = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)
      _     <- torpor.query(schema, "{ items { detail { id } } }") match {
                 case Left(error) => IO(fail(error.render.mkString("\n")))
                 case Right(io)   => io
               }
      seen  <- nodes.get
      count <- query.get
    } yield {
      assertEquals(count, 1)
      assertEquals(
        seen.sortBy(_.render),
        List(FieldCoordinate("GuardedItem", "detail"), FieldCoordinate("Query", "items"))
      )
    }
  }
}
