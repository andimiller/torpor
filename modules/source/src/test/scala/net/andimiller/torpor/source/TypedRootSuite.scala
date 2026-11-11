package net.andimiller.torpor.source

import cats.effect.IO
import io.circe.Decoder
import net.andimiller.torpor.core.{Argument, FieldCoordinate, Resolver, Value}

final class TypedRootSuite extends munit.CatsEffectSuite {

  private type PetsArgs = Arguments[(("species", Option[String]), ("first", Option[Int]))] {
    val species: Option[String]; val first: Option[Int]
  }

  // what the schema macro would emit for `pets(species: String, first: Int): [String!]!`
  private val pets = FieldCoordinate("Query", "pets").asInstanceOf[FieldCoordinate.Of["Query", "pets", PetsArgs]]

  private def run(entry: (FieldCoordinate, Resolver[IO]), arguments: Argument*): IO[io.circe.Json] =
    entry match {
      case (_, Resolver.Root(run)) => run(arguments.toList)
      case (coordinate, other)     => IO(fail(s"${coordinate.render} registered $other, not a root resolver"))
    }

  test("a typed coordinate types the lambda's arguments, so no arguments class is needed") {
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

  test("a plain coordinate still names its argument type") {
    final case class Species(species: Option[String]) derives Decoder

    val entry = resolver.root[IO]("Query" / "pets") { (args: Species) => IO(args.species.getOrElse("any")) }
    run(entry, Argument("species", Value.StringValue("CAT"))).map(json => assertEquals(json.as[String], Right("CAT")))
  }
}
