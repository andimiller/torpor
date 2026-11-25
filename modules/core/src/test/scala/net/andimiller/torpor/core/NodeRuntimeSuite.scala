package net.andimiller.torpor.core

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json

final class NodeRuntimeSuite extends munit.FunSuite {

  private final case class Pet(name: String, ownerId: String)

  private val pets  = FieldCoordinate("Query", "pets")
  private val owner = FieldCoordinate("Pet", "owner")

  private val root  =
    PlanNode(NodeId("n0"), "pets", pets, Nil, SelectionSet(List(NodeField("owner", "owner", NodeId("n1")))), Nil, 1)
  private val child = PlanNode(NodeId("n1"), "owners", owner, Nil, SelectionSet.empty, List(NodeId("n0")), 2)
  private val plan  = Plan("Query", SelectionSet(List(NodeField("pets", "pets", NodeId("n0")))), List(root, child))

  private val rex     = Pet("Rex", "o1")
  private val rexJson = Json.obj("name" -> Json.fromString("Rex"))

  // the resolver's own value is wrapped in `Option`, as a nullable list field's would be
  private val resolver = Resolver.Root[IO](_ => IO.pure(Resolved(Json.arr(rexJson), Some(List(rex)))))

  private val ownerPath = List(PathSegment.Field("pets"), PathSegment.Index(0), PathSegment.Field("owner"))

  private def parentsOfOwner(hooks: Hooks[IO]): List[(List[PathSegment], Parent)] =
    NodeRuntime.run[IO](plan, root, None, resolver, Some("pets"), hooks).unsafeRunSync() match {
      case NodeOutcome.Completed(result) => NodeRuntime.inputPositions(plan, child, result)
      case other                         => fail(s"expected a result, got $other")
    }

  /** Replaces every value of `Query.pets` with what `rewrite` makes of its JSON. */
  private def rewriting(rewrite: Json => Json): Hooks[IO] = new Hooks[IO] {
    override def authorize(
        coordinate: FieldCoordinate,
        args: List[Argument],
        values: Map[String, Json]
    ): IO[Map[String, Either[HookError, Json]]] =
      IO.pure(values.map { case (key, value) => key -> Right(rewrite(value)) })
  }

  test("a keyed child gets the Scala value that its parent node produced, through options and lists") {
    assertEquals(parentsOfOwner(Hooks.noop[IO]), List(ownerPath -> Parent(rexJson, Some(Produced(pets, rex)))))
  }

  test("a hook that keeps a value keeps its Scala value") {
    assertEquals(parentsOfOwner(rewriting(identity)), List(ownerPath -> Parent(rexJson, Some(Produced(pets, rex)))))
  }

  test("a hook that changes a value drops its Scala value, so the child reads the new JSON") {
    val mochi = Json.obj("name" -> Json.fromString("Mochi"))
    assertEquals(parentsOfOwner(rewriting(_ => Json.arr(mochi))), List(ownerPath -> Parent(mochi, None)))
  }
}
