package net.andimiller.torpor.source

import cats.effect.IO
import io.circe.Decoder
import net.andimiller.torpor.core.{Argument, Resolver, Value}
import net.andimiller.torpor.macros.schema

final class RootResolverSuite extends munit.CatsEffectSuite {

  private val pets = schema"""extend type Query { pets(species: String, first: Int): [String!]! }""".Query.pets

  private def run(resolver: FieldResolver[IO, ?], arguments: Argument*): IO[io.circe.Json] =
    resolver.entry match {
      case (_, Resolver.Root(run)) => run(arguments.toList)
      case (coordinate, other)     => IO(fail(s"${coordinate.render} registered $other, not a root resolver"))
    }

  test("the SDL types the lambda's arguments") {
    val entry = resolver.root[IO](pets) { args => IO(args.species.toList ++ args.first.map(_.toString)) }

    run(entry, Argument("species", Value.StringValue("DOG")), Argument("first", Value.IntValue(2)))
      .map(json => assertEquals(json.as[List[String]], Right(List("DOG", "2"))))
  }

  test("absent nullable arguments are None") {
    val entry = resolver.root[IO](pets) { args => IO(args.species.isEmpty && args.first.isEmpty) }
    run(entry).map(json => assertEquals(json.as[Boolean], Right(true)))
  }

  test("args overrides the SDL's typing with a decoder of your own") {
    final case class Species(species: Option[String]) derives Decoder

    val entry = resolver.root[IO](pets).args[Species] { args => IO(args.species.getOrElse("any")) }
    run(entry).map(json => assertEquals(json.as[String], Right("any")))
  }
}
