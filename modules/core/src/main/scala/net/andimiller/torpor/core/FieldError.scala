package net.andimiller.torpor.core

import cats.data.Chain
import io.circe.Json

/** A position in the response, for error paths. Field names are response keys (aliases), not schema names. */
enum PathSegment {
  case Field(name: String)
  case Index(index: Int)
}

/** A per-field error. Never an exception: the executor converts failures into these so partial results survive
  * (invariant 1).
  */
final case class FieldError(message: String, path: List[PathSegment], code: String) {
  def toJson: Json =
    Json.obj(
      "message"    -> Json.fromString(message),
      "path"       -> Json.fromValues(path.map {
        case PathSegment.Field(name)  => Json.fromString(name)
        case PathSegment.Index(index) => Json.fromInt(index)
      }),
      "extensions" -> Json.obj("code" -> Json.fromString(code))
    )
}

object FieldError {
  val MissingKey          = "MISSING_KEY"
  val InvalidParentValue  = "INVALID_PARENT_VALUE"
  val UpstreamUnavailable = "UPSTREAM_UNAVAILABLE"
}

/** A GraphQL response. `data` is always present; `errors` carries field errors with their response paths. */
final case class Response(data: Json, errors: Chain[FieldError]) {
  def toJson: Json =
    if (errors.isEmpty) Json.obj("data" -> data)
    else Json.obj("data"                -> data, "errors" -> Json.fromValues(errors.toList.map(_.toJson)))
}
