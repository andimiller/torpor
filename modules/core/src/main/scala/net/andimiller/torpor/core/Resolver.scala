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
    * Keys are strings because a batch is keyed by equality and the key has to survive a JSON round trip. The typed DSL
    * encodes them with circe; a key that cannot round-trip is a registration error, not a query error.
    */
  case Batch(key: Json => Either[String, String], batch: NonEmptyList[String] => F[Map[String, Json]])

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
