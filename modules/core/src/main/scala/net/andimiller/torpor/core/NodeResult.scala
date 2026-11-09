package net.andimiller.torpor.core

import cats.data.Chain
import io.circe.Json

/** What a node produced: a resolved value per response position, plus the field errors raised while producing them.
  *
  * Values are keyed by response path so assembly can find each child value without guessing shapes.
  */
final case class NodeResult(values: List[(List[PathSegment], Json)], errors: Chain[FieldError])
