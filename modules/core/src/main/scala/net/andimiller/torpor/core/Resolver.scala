package net.andimiller.torpor.core

import cats.data.NonEmptyList
import io.circe.Json

/** What the executor needs to run one plan node. This keeps `torpor-core` free of the schema/parser modules.
  *
  * `torpor-source` builds these from typed resolvers; the executor holds their values as `Any` and never looks inside.
  */
enum Resolver[F[_]] {

  /** A root field: one call, no input values. Arguments are already literal (the planner substituted variables). */
  case Root(run: List[Argument] => F[Resolved])

  /** A keyed field: extract a key from each parent value, then one batch call for all of them.
    *
    * Arguments are the node's own, already literal and shared by every parent in the node (the planner gives each
    * distinct argument set its own node), so the batch sees them once.
    *
    * The executor calls `batch` once with the distinct keys, by Scala equality, and matches each result to its parents
    * by the same equality, so a key needs no codec. `None` is a parent with no key, which resolves to `null` without a
    * call. The executor never names `K`: it only passes what `key` returns to `batch`.
    */
  case Batch[F[_], K](
      key: Parent => Either[String, Option[K]],
      batch: (List[Argument], NonEmptyList[K]) => F[Map[K, Resolved]]
  ) extends Resolver[F]

  /** A root field that can park the run. `start` either completes or suspends with a handle; `resume` gets that handle
    * and the webhook payload back and may complete or suspend again.
    *
    * Handles and payloads are JSON because they have to survive a snapshot round trip; the typed DSL encodes them.
    */
  case Suspending(
      start: List[Argument] => F[Step[Resolved, Json]],
      resume: (Json, Json) => F[Step[Resolved, Json]]
  )
}

/** What a resolver produced: its JSON, for the response and for snapshots, and its Scala value, for the keyed fields
  * that read it later in the same run.
  */
final case class Resolved(json: Json, value: Any)

/** The parent value that a keyed field reads its key from. `produced` is there when a resolver in this run produced the
  * value itself; it is absent for a value nested inside another value, and for one restored from a snapshot.
  */
final case class Parent(json: Json, produced: Option[Produced])

/** A value as the resolver at `by` produced it, before encoding. */
final case class Produced(by: FieldCoordinate, value: Any)
