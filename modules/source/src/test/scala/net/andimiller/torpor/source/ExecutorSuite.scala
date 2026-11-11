package net.andimiller.torpor.source

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.{Decoder, Encoder, Json}
import net.andimiller.torpor.core.{FieldError, PathSegment, Plan, Response}
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.schema.Executable

import java.util.concurrent.atomic.AtomicInteger

/** Opaque ids are the documented pattern: one given pair per id type, or `derives` will not find them. */
opaque type OwnerId = String
object OwnerId {
  def apply(value: String): OwnerId = value
  given Encoder[OwnerId]            = Encoder.encodeString.contramap[OwnerId](identity)
  given Decoder[OwnerId]            = Decoder.decodeString.map(OwnerId(_))
}

final case class Pet(id: String, name: String, species: String, ownerId: OwnerId) derives Encoder.AsObject, Decoder

final case class Owner(id: OwnerId, name: String, email: String) derives Encoder.AsObject, Decoder

final case class ReportArgs(species: String) derives Decoder
final case class AdoptionReport(species: String, adoptions: Int) derives Encoder.AsObject, Decoder

final class ExecutorSuite extends munit.CatsEffectSuite {

  private val pets: List[Pet] = List(
    Pet("p1", "Rex", "DOG", OwnerId("o1")),
    Pet("p2", "Mochi", "CAT", OwnerId("o1")),
    Pet("p3", "Biscuit", "DOG", OwnerId("o2"))
  )

  private val owners: List[Owner] = List(
    Owner(OwnerId("o1"), "Ada", "ada@example.com"),
    Owner(OwnerId("o2"), "Grace", "grace@example.com")
  )

  private val petsSdl =
    schema"""enum Species { DOG CAT }
      |type Pet { id: ID! name: String! species: Species! }
      |extend type Query { pets(species: Species, first: Int = 20): [Pet!]! }
      |"""

  private val ownersSdl =
    schema"""type Owner { id: ID! name: String! email: String! }
      |extend type Pet { owner: Owner! }
      |"""

  private val reportsSdl =
    schema"""type AdoptionReport { species: Species! adoptions: Int! }
      |extend type Query { adoptionReport(species: Species!): AdoptionReport! }
      |"""

  private final class Calls {
    val pets       = new AtomicInteger(0)
    val owners     = new AtomicInteger(0)
    val reports    = new AtomicInteger(0)
    val ownersKeys = new AtomicInteger(0)
  }

  private final class Fixture(ownersFail: Boolean = false, omitOwner: Option[OwnerId] = None) {
    val calls = new Calls

    private val petsSource = Source[IO]("pets", petsSdl.text)(
      resolver.root[IO](petsSdl.Query.pets) { args =>
        IO {
          calls.pets.incrementAndGet()
          pets.filter(p => args.species.forall(_ == p.species)).take(args.first.getOrElse(20))
        }
      }
    )

    private val ownersSource = Source[IO]("owners", ownersSdl.text)(
      resolver
        .field[IO](ownersSdl.Pet.owner)
        .key((pet: Pet) => pet.ownerId)
        .batch { (keys: NonEmptyList[OwnerId]) =>
          IO {
            calls.owners.incrementAndGet()
            calls.ownersKeys.set(keys.size)
            if (ownersFail) throw new RuntimeException("owners service unavailable")
            owners
              .filter(owner => keys.toList.contains(owner.id) && !omitOwner.contains(owner.id))
              .map(owner => owner.id -> owner)
              .toMap
          }
        }
    )

    // `Species` is declared by the pets source, so this fragment cannot type the argument: decode it as a class
    private val reportsSource = Source[IO]("reports", reportsSdl.text)(
      resolver.root[IO](reportsSdl.Query.adoptionReport).args[ReportArgs] { args =>
        IO {
          calls.reports.incrementAndGet()
          AdoptionReport(args.species, 42)
        }
      }
    )

    val torpor = Torpor[IO](petsSource, ownersSource, reportsSource)

    val schema = torpor.schema.fold(
      errors => fail(errors.toChain.toList.map(_.render).mkString("; ")),
      identity
    )

    def plan(query: String): Plan =
      Executable.validate(schema, query) match {
        case Left(errors)     => fail(errors.toChain.toList.map(_.render).mkString("\n"))
        case Right(validated) =>
          Planner.build(schema, validated) match {
            case Left(problems) => fail(problems.toChain.toList.map(_.render).mkString("\n"))
            case Right(plan)    => plan
          }
      }

    def execute(query: String): IO[Response] = torpor.run(plan(query))
  }

  private def fieldNames(cursor: io.circe.ACursor): List[String] =
    cursor.keys.map(_.toList).getOrElse(Nil)

  test("one call per node per level, with keys deduped") {
    val fixture = new Fixture
    fixture
      .execute("{ pets { name owner { name email } } }")
      .map { response =>
        assertEquals(fixture.calls.pets.get, 1)
        assertEquals(fixture.calls.owners.get, 1)
        assertEquals(fixture.calls.ownersKeys.get, 2, "three pets share two owners, so two keys go to the batch")

        val cursor = response.data.hcursor
        assertEquals(cursor.downField("pets").downArray.get[String]("name").toOption, Some("Rex"))
        assertEquals(
          cursor.downField("pets").downArray.downField("owner").get[String]("name").toOption,
          Some("Ada")
        )
        assertEquals(
          cursor.downField("pets").downN(2).downField("owner").get[String]("name").toOption,
          Some("Grace")
        )
        assert(response.errors.isEmpty)
      }
  }

  test("aliases key the response while the plan keeps field coordinates") {
    val fixture = new Fixture
    fixture.execute("{ dogs: pets { name } }").map { response =>
      assertEquals(fieldNames(response.data.hcursor), List("dogs"))
      assertEquals(response.data.hcursor.downField("dogs").downArray.get[String]("name").toOption, Some("Rex"))
    }
  }

  test("independent root fields are both resolved in one run") {
    val fixture = new Fixture
    fixture
      .execute("""{ pets { name } adoptionReport(species: DOG) { adoptions } }""")
      .map { response =>
        assertEquals(fixture.calls.pets.get, 1)
        assertEquals(fixture.calls.reports.get, 1)
        assertEquals(
          response.data.hcursor.downField("adoptionReport").get[Int]("adoptions").toOption,
          Some(42)
        )
      }
  }

  test("an upstream failure is a field error at the right paths, with partial data") {
    val fixture = new Fixture(ownersFail = true)
    fixture.execute("{ pets { name owner { name } } }").map { response =>
      assertEquals(fixture.calls.owners.get, 1)

      val cursor = response.data.hcursor
      // partial data survives: names are still there, only the failed field is null
      assertEquals(cursor.downField("pets").downArray.get[String]("name").toOption, Some("Rex"))
      assert(cursor.downField("pets").downArray.downField("owner").focus.contains(Json.Null))

      assertEquals(response.errors.toList.size, 3, "one error per pet position")
      assertEquals(
        response.errors.toList.head.path,
        List(PathSegment.Field("pets"), PathSegment.Index(0), PathSegment.Field("owner"))
      )
      assertEquals(response.errors.toList.head.code, FieldError.UpstreamUnavailable)
      assert(response.errors.toList.head.message.contains("owners service unavailable"))

      // spec-shaped response: data and errors side by side
      val json = response.toJson
      assertEquals(fieldNames(json.hcursor).sorted, List("data", "errors"))
      assertEquals(
        json.hcursor.downField("errors").downArray.get[List[Json]]("path").toOption.map(_.map(_.noSpaces)),
        Some(List("\"pets\"", "0", "\"owner\""))
      )
    }
  }

  test("a missing key is an error at that position, not a failed run") {
    val fixture = new Fixture(omitOwner = Some(OwnerId("o2")))
    fixture.execute("{ pets { name owner { name } } }").map { response =>
      val errors = response.errors.toList
      assertEquals(errors.size, 1)
      assertEquals(errors.head.code, FieldError.MissingKey)
      assertEquals(
        errors.head.path,
        List(PathSegment.Field("pets"), PathSegment.Index(2), PathSegment.Field("owner"))
      )
      assert(
        response.data.hcursor
          .downField("pets")
          .downN(2)
          .downField("owner")
          .focus
          .contains(Json.Null)
      )
      // the other two owners still resolved
      assertEquals(
        response.data.hcursor.downField("pets").downArray.downField("owner").get[String]("name").toOption,
        Some("Ada")
      )
    }
  }

  test("a resolver that throws synchronously becomes a field error, not a failed run") {
    val fixture = new Fixture
    // `first` will not decode as Int, so the DSL throws while building the effect: the boundary must catch it
    fixture.execute("""{ pets(first: "not-a-number") { name } }""").map { response =>
      assert(response.data.hcursor.downField("pets").focus.contains(Json.Null))
      assertEquals(response.errors.toList.size, 1)
      assert(
        response.errors.toList.head.message.contains("cannot decode arguments"),
        response.errors.toList.head.message
      )
      assertEquals(response.errors.toList.head.code, FieldError.UpstreamUnavailable)
    }
  }

  test("the request path through the facade runs end to end") {
    val fixture = new Fixture
    val result  = fixture.torpor.query(fixture.schema, "{ pets(first: 1) { name } }")
    result match {
      case Left(errors) => fail(errors.render.mkString("\n"))
      case Right(io)    =>
        io.map { response =>
          assertEquals(response.data.hcursor.downField("pets").downArray.get[String]("name").toOption, Some("Rex"))
        }
    }
  }
}
