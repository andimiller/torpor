package net.andimiller.torpor.source

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.{Decoder, Encoder, Json}
import net.andimiller.torpor.core.{FieldError, PathSegment}
import net.andimiller.torpor.schema.Schema
import net.andimiller.torpor.macros.schema

final case class BubblingParent(id: String) derives Encoder.AsObject, Decoder
final case class BubblingChild(id: String) derives Encoder.AsObject, Decoder

/** A null in a non-null position nulls the nearest nullable ancestor and keeps the error at the original path. */
final class NullBubblingSuite extends munit.CatsEffectSuite {

  private def run(torpor: Torpor[IO], schema: Schema)(query: String) =
    torpor.query(schema, query) match {
      case Left(error) => IO(fail(error.render.mkString("\n")))
      case Right(io)   => io
    }

  private val nullableSdl = schema"""type BubblingChild { id: ID! }
    |type BubblingParent { id: ID! }
    |extend type BubblingParent { child: BubblingChild }
    |extend type Query { parents: [BubblingParent]! }"""

  private val nullableChild = Source[IO]("bubbling", nullableSdl)(
    resolver.root[IO](nullableSdl.Query.parents) { _ =>
      IO.pure(List(BubblingParent("p1"), BubblingParent("p2"), BubblingParent("p3")))
    },
    resolver
      .field[IO](nullableSdl.BubblingParent.child)
      .key((parent: BubblingParent) => parent.id)
      .batch { (_, keys: NonEmptyList[String]) =>
        IO.pure(keys.toList.filterNot(_ == "p2").map(id => id -> BubblingChild(s"$id-child")).toMap)
      }
  )

  private val nullableParentSdl = schema"""type BubblingChild { id: ID! }
    |type BubblingParent { id: ID! }
    |extend type BubblingParent { child: BubblingChild! }
    |extend type Query { parents: [BubblingParent]! }"""

  private val nullableParentChild = Source[IO]("bubbling", nullableParentSdl)(
    resolver.root[IO](nullableParentSdl.Query.parents) { _ =>
      IO.pure(List(BubblingParent("p1"), BubblingParent("p2"), BubblingParent("p3")))
    },
    resolver
      .field[IO](nullableParentSdl.BubblingParent.child)
      .key((parent: BubblingParent) => parent.id)
      .batch { (_, keys: NonEmptyList[String]) =>
        IO.pure(keys.toList.filterNot(_ == "p2").map(id => id -> BubblingChild(s"$id-child")).toMap)
      }
  )

  private val nonNullSdl = schema"""type BubblingChild { id: ID! }
    |type BubblingParent { id: ID! }
    |extend type BubblingParent { child: BubblingChild! }
    |extend type Query { parents: [BubblingParent!]! }"""

  private val nonNullChild = Source[IO]("bubbling", nonNullSdl)(
    resolver.root[IO](nonNullSdl.Query.parents) { _ =>
      IO.pure(List(BubblingParent("p1"), BubblingParent("p2"), BubblingParent("p3")))
    },
    resolver
      .field[IO](nonNullSdl.BubblingParent.child)
      .key((parent: BubblingParent) => parent.id)
      .batch { (_, keys: NonEmptyList[String]) =>
        IO.pure(keys.toList.filterNot(_ == "p2").map(id => id -> BubblingChild(s"$id-child")).toMap)
      }
  )

  test("a non-null child that is null nulls that parent and keeps the error path") {
    val torpor = Torpor[IO](nullableParentChild)
    val schema = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)
    run(torpor, schema)("{ parents { id child { id } } }").map { response =>
      val parents = response.data.hcursor.downField("parents")
      assertEquals(parents.downN(0).downField("child").get[String]("id").toOption, Some("p1-child"))
      assert(parents.downN(1).focus.contains(Json.Null), "the parent with the bad child is nulled")
      assertEquals(parents.downN(2).downField("child").get[String]("id").toOption, Some("p3-child"))

      val errors = response.errors.toList
      assertEquals(errors.size, 1, "the missing key is not reported twice as a null-in-non-null")
      assertEquals(errors.head.code, FieldError.MissingKey)
      assertEquals(
        errors.head.path,
        List(PathSegment.Field("parents"), PathSegment.Index(1), PathSegment.Field("child"))
      )
    }
  }

  test("a nullable child that is null stays null without an extra error") {
    val torpor = Torpor[IO](nullableChild)
    val schema = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)
    run(torpor, schema)("{ parents { id child { id } } }").map { response =>
      assert(response.data.hcursor.downField("parents").downN(1).downField("child").focus.contains(Json.Null))
      assertEquals(response.errors.toList.map(_.code), List(FieldError.MissingKey))
    }
  }

  test("bubbling past the root makes data null") {
    val noChildren = Source[IO]("bubbling", nonNullSdl)(
      resolver.root[IO](nonNullSdl.Query.parents) { _ =>
        IO.pure(List(BubblingParent("p1")))
      },
      resolver
        .field[IO](nonNullSdl.BubblingParent.child)
        .key((parent: BubblingParent) => parent.id)
        .batch((_, _: NonEmptyList[String]) => IO.pure(Map.empty[String, BubblingChild]))
    )
    val torpor     = Torpor[IO](noChildren)
    val schema     = torpor.schema.fold(errors => fail(errors.toChain.toList.map(_.render).mkString("; ")), identity)

    run(torpor, schema)("{ parents { child { id } } }").map { response =>
      assert(response.data.isNull, "a non-null root field bubbled to the whole response")
      assertEquals(response.errors.toList.map(_.code), List(FieldError.MissingKey))
    }
  }
}
