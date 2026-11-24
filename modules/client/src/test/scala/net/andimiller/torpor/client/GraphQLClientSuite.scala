package net.andimiller.torpor.client

import cats.effect.IO
import io.circe.{Decoder, Json}
import net.andimiller.torpor.http.TorporHttp
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.source.*
import org.http4s.{HttpApp, Response, Status}
import org.http4s.circe.*
import org.http4s.client.{Client, UnexpectedStatus}
import org.http4s.implicits.*

/** A value class, read by its own decoder where the API binds it. */
final case class PetId(value: String) extends AnyVal

object PetId {
  given Decoder[PetId] = Decoder.decodeString.map(PetId(_))
}

/** Typed operations run against a real torpor server, in process. */
final class GraphQLClientSuite extends munit.CatsEffectSuite {

  private val sdl = schema"""enum Species { DOG CAT }
    |type Pet { id: ID! name: String! species: Species! age: Int owner: Owner }
    |type Owner { name: String! pets: [Pet!]! }
    |type Shelter { name: String! capacity: Int! }
    |union Home = Owner | Shelter
    |extend type Query {
    |  pets(species: Species, first: Int): [Pet!]!
    |  pet(id: ID!): Pet
    |  home(pet: ID!): Home
    |}"""

  private def pet(id: String, name: String, age: Option[Int]): Json =
    Json.obj(
      "id"      -> Json.fromString(id),
      "name"    -> Json.fromString(name),
      "species" -> Json.fromString("DOG"),
      "age"     -> age.fold(Json.Null)(Json.fromInt),
      "owner"   -> Json.obj("name" -> Json.fromString("Ada"), "pets" -> Json.arr())
    )

  private val pets = List(pet("1", "Rex", Some(3)), pet("2", "Fido", None), pet("3", "Bo", Some(7)))

  private val source = Source[IO]("pets", sdl)(
    resolver.root[IO](sdl.Query.pets)(args => IO.pure(pets.take(args.first.getOrElse(pets.size)))),
    resolver.root[IO](sdl.Query.pet) { args =>
      if (args.id == "broken") IO.raiseError(new RuntimeException("the kennel is closed"))
      else IO.pure(pets.find(_.hcursor.get[String]("id").contains(args.id)))
    },
    resolver.root[IO](sdl.Query.home) { _ =>
      IO.pure(
        Option(
          Json.obj(
            "__typename" -> Json.fromString("Shelter"),
            "name"       -> Json.fromString("Paws"),
            "capacity"   -> Json.fromInt(40)
          )
        )
      )
    }
  )

  private val client: IO[GraphQLClient[IO]] =
    Torpor[IO](source).startup.map { (torpor, schema) =>
      new GraphQLClient[IO](Client.fromHttpApp(TorporHttp.routes(torpor, schema).orNotFound), uri"/graphql")
    }

  private val petshop = Api.sdl(PetshopSchema.sdl)

  private val onePet = petshop.operation("query P($id: ID!) { pet(id: $id) { name } }")

  test("errors in the response are raised with their paths") {
    for {
      graphql <- client
      failure <- graphql.run(onePet(id = "broken")).attempt
    } yield failure match {
      case Left(raised: GraphQLFailure) => assertEquals(raised.errors.map(_.path), List(List("pet")))
      case other                        => fail(s"expected a GraphQLFailure, got $other")
    }
  }

  test("errors in a response that is not a 200 are raised all the same") {
    val refused = Json.obj("errors" -> Json.arr(Json.obj("message" -> Json.fromString("no such operation"))))
    val graphql = new GraphQLClient[IO](
      Client.fromHttpApp(HttpApp.pure[IO](Response[IO](Status.BadRequest).withEntity(refused))),
      uri"/graphql"
    )

    graphql.run(onePet(id = "1")).attempt.map {
      case Left(raised: GraphQLFailure) => assertEquals(raised.errors.map(_.message), List("no such operation"))
      case other                        => fail(s"expected a GraphQLFailure, got $other")
    }
  }

  test("a failure that is not a GraphQL response is raised with its status") {
    val graphql = new GraphQLClient[IO](
      Client.fromHttpApp(HttpApp.pure[IO](Response[IO](Status.ServiceUnavailable).withEntity("<html>down</html>"))),
      uri"/graphql"
    )

    graphql.run(onePet(id = "1")).attempt.map {
      case Left(raised: UnexpectedStatus) => assertEquals(raised.status, Status.ServiceUnavailable)
      case other                          => fail(s"expected an UnexpectedStatus, got $other")
    }
  }

  test("a typed operation's response is read by field, with the types the schema gives") {
    val pets = petshop.operation(
      """
        query Pets($first: Int) {
          pets(first: $first) {
            name
            age
            owner { name }
          }
        }
      """
    )

    for {
      graphql <- client
      data    <- graphql.run(pets(first = Some(2)))
    } yield {
      val read: List[(String, Option[Int], Option[String])] =
        data.pets.map(pet => (pet.name, pet.age, pet.owner.map(_.name)))
      assertEquals(read, List(("Rex", Some(3), Some("Ada")), ("Fido", None, Some("Ada"))))
    }
  }

  test("a typed operation with no variables is called with none") {
    val everyPet = petshop.operation("{ pets { name } }")

    for {
      graphql <- client
      data    <- graphql.run(everyPet())
    } yield assertEquals(data.pets.map(_.name), List("Rex", "Fido", "Bo"))
  }

  test("a typed operation reads a field as the type the API binds for that field") {
    val pets = petshop.read["Pet.id", PetId].operation("{ pets { id } }")

    for {
      graphql <- client
      data    <- graphql.run(pets())
    } yield {
      val ids: List[PetId] = data.pets.map(_.id)
      assertEquals(ids, List(PetId("1"), PetId("2"), PetId("3")))
    }
  }

  test("a typed operation reads a leaf as the type the API binds for its type") {
    val pets = petshop.read["Int", Long].operation("{ pets { age } }")

    for {
      graphql <- client
      data    <- graphql.run(pets())
    } yield {
      val ages: List[Option[Long]] = data.pets.map(_.age)
      assertEquals(ages, List(Some(3L), None, Some(7L)))
    }
  }

}
