package net.andimiller.torpor.core

import cats.data.NonEmptyList
import io.circe.Json

/** What the executor needs to run one plan node. The SPI is JSON-shaped on purpose: the plan is JSON-shaped, snapping
  * happens inside the executor, and this keeps `torpor-core` free of the schema/parser modules.
  *
  * `torpor-source` builds these from typed resolvers; nothing in the executor sees a user type.
  */
enum Resolver[F[_]] {

  /** A root field: one call, no input values. Arguments are already literal (the planner substituted variables). */
  case Root(run: List[Argument] => F[Json])

  /** A keyed field: extract a key from each parent value, then one batch call for all of them.
    *
    * Arguments are the node's own, already literal and shared by every parent in the node (the planner gives each
    * distinct argument set its own node), so the batch sees them once.
    *
    * A batch is keyed by each key's JSON encoding, which is also what `Hooks.authorize` sees; `batch` gets the keys as
    * the Scala values `key` extracted, so they never need to decode. `None` is a parent with no key, which resolves to
    * `null` without a call. The executor never names `K`: it only passes what `key` returns to `batch`.
    */
  case Batch[F[_], K](
      key: Json => Either[String, Option[Key[K]]],
      batch: (List[Argument], NonEmptyList[K]) => F[Map[String, Json]]
  ) extends Resolver[F]

  /** A root field that can park the run. `start` either completes or suspends with a handle; `resume` gets that handle
    * and the webhook payload back and may complete or suspend again.
    *
    * Handles and payloads are JSON because they have to survive a snapshot round trip; the typed DSL encodes them.
    */
  case Suspending(
      start: List[Argument] => F[Step[Json, Json]],
      resume: (Json, Json) => F[Step[Json, Json]]
  )
}

/** A batch key: its JSON encoding, which identifies it, and the Scala value the resolver extracted. */
final case class Key[K](encoded: String, value: K)
