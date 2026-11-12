package net.andimiller.torpor.source

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Ref
import io.circe.{Decoder, Encoder}
import net.andimiller.torpor.macros.schema

final case class Workspace(id: String) derives Encoder.AsObject, Decoder
final case class Cohort(id: String) derives Encoder.AsObject, Decoder

/** Arguments on a batched field reach the batch once per argument set. */
final class BatchArgsSuite extends munit.CatsEffectSuite {

  private val sdl = schema"""type Cohort { id: ID! }
    |type Workspace { id: ID! }
    |extend type Workspace { cohorts(first: Int, after: String): [Cohort!]! }
    |extend type Query { workspaces: [Workspace!]! }"""

  private def fixture
      : IO[(Torpor[IO], net.andimiller.torpor.schema.Schema, Ref[IO, List[(Option[Int], List[String])]])] =
    for {
      calls <- Ref.of[IO, List[(Option[Int], List[String])]](Nil)
      source = Source[IO]("workspaces", sdl)(
                 resolver.root[IO](sdl.Query.workspaces) { _ =>
                   IO.pure(List(Workspace("w1"), Workspace("w2")))
                 },
                 resolver
                   .field[IO](sdl.Workspace.cohorts)
                   .key((workspace: Workspace) => workspace.id)
                   .batch { (args, keys: NonEmptyList[String]) =>
                     calls.update(_ :+ (args.first, keys.toList)) *> IO.pure(
                       keys.toList.map { id =>
                         id -> List(Cohort(s"$id-c1"), Cohort(s"$id-c2"), Cohort(s"$id-c3"))
                           .take(args.first.getOrElse(3))
                       }.toMap
                     )
                   }
               )
      torpor = Torpor[IO](source)
      schema = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)
    } yield (torpor, schema, calls)

  test("two argument sets make two calls, each with its own args and every parent key") {
    fixture.flatMap { case (torpor, schema, calls) =>
      torpor.query(
        schema,
        """{ workspaces { id two: cohorts(first: 2) { id } one: cohorts(first: 1) { id } } }"""
      ) match {
        case Left(error) => IO(fail(error.render.mkString("\n")))
        case Right(io)   =>
          io.flatMap { response =>
            calls.get.map { recorded =>
              assertEquals(recorded.sortBy(_._1), List(Some(1) -> List("w1", "w2"), Some(2) -> List("w1", "w2")))

              val workspaces = response.data.hcursor.downField("workspaces")
              assertEquals(workspaces.downArray.downField("two").values.map(_.size), Some(2))
              assertEquals(workspaces.downArray.downField("one").values.map(_.size), Some(1))
              assertEquals(response.errors.toList, Nil)
            }
          }
      }
    }
  }

  test("an omitted argument stays omitted rather than guessed") {
    fixture.flatMap { case (torpor, schema, calls) =>
      torpor.query(schema, """{ workspaces { cohorts { id } } }""") match {
        case Left(error) => IO(fail(error.render.mkString("\n")))
        case Right(io)   =>
          io.flatMap { response =>
            calls.get.map { recorded =>
              assertEquals(recorded, List(None -> List("w1", "w2")))
              assertEquals(
                response.data.hcursor.downField("workspaces").downArray.downField("cohorts").values.map(_.size),
                Some(3)
              )
            }
          }
      }
    }
  }
}
