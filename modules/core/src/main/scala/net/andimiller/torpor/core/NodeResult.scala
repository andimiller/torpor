package net.andimiller.torpor.core

import cats.data.Chain
import io.circe.Json

/** What a node produced: a resolved value per response position, plus the field errors raised while producing them.
  *
  * Values are keyed by response path so assembly can find each child value without guessing shapes. `produced` holds
  * the same values as their resolver's Scala values, for keyed child nodes in this run; snapshots do not store it.
  */
final case class NodeResult(
    values: List[(List[PathSegment], Json)],
    errors: Chain[FieldError],
    produced: Map[List[PathSegment], Any] = Map.empty
)
