package net.andimiller.torpor.snapshot

import cats.data.Chain
import cats.syntax.all._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._
import net.andimiller.torpor.core.*

/** JSON codecs for everything a snapshot holds.
  *
  * The shape is internal but versioned (`SnapshotEnvelope.Version`); a resumed run is refused if the version or the
  * schema hash does not match. Hand-written rather than derived so the plan's encoding cannot change silently.
  */
object Codecs {

  implicit val nodeIdEncoder: Encoder[NodeId]              = Encoder[String].contramap(_.value)
  implicit val nodeIdDecoder: Decoder[NodeId]              = Decoder[String].map(NodeId(_))
  implicit val coordinateEncoder: Encoder[FieldCoordinate] =
    Encoder.instance(c => Json.obj("parentType" := c.parentType, "field" := c.field))
  implicit val coordinateDecoder: Decoder[FieldCoordinate] =
    Decoder.instance(c => (c.get[String]("parentType"), c.get[String]("field")).mapN(FieldCoordinate(_, _)))

  implicit val valueEncoder: Encoder[Value] = Encoder.instance {
    case Value.IntValue(v)      => Json.obj("t" := "int", "v" := v.toString)
    case Value.FloatValue(v)    => Json.obj("t" := "float", "v" := v.toString)
    case Value.StringValue(v)   => Json.obj("t" := "string", "v" := v)
    case Value.BooleanValue(v)  => Json.obj("t" := "bool", "v" := v)
    case Value.EnumValue(v)     => Json.obj("t" := "enum", "v" := v)
    case Value.NullValue        => Json.obj("t" := "null")
    case Value.ListValue(vs)    => Json.obj("t" := "list", "v" := vs.asJson)
    case Value.ObjectValue(fs)  =>
      Json.obj("t" := "object", "v" := fs.map { case (k, v) => Json.obj("k" := k, "v" := v) }.asJson)
    case Value.VariableValue(n) => Json.obj("t" := "variable", "v" := n)
  }

  implicit val valueDecoder: Decoder[Value] = Decoder.instance { c =>
    c.get[String]("t").flatMap {
      case "int"      => c.get[String]("v").map(s => BigInt(s)).map(Value.IntValue(_))
      case "float"    => c.get[String]("v").map(s => BigDecimal(s)).map(Value.FloatValue(_))
      case "string"   => c.get[String]("v").map(Value.StringValue(_))
      case "bool"     => c.get[Boolean]("v").map(Value.BooleanValue(_))
      case "enum"     => c.get[String]("v").map(Value.EnumValue(_))
      case "null"     => Right(Value.NullValue)
      case "list"     => c.get[List[Value]]("v").map(Value.ListValue(_))
      case "object"   =>
        c.get[List[Json]]("v")
          .flatMap(_.traverse { entry =>
            (entry.hcursor.get[String]("k"), entry.hcursor.get[Value]("v")).mapN(_ -> _)
          })
          .map(Value.ObjectValue(_))
      case "variable" => c.get[String]("v").map(Value.VariableValue(_))
      case other      => Left(DecodingFailure(s"unknown value kind $other", c.history))
    }
  }

  implicit val argumentEncoder: Encoder[Argument] =
    Encoder.instance(a => Json.obj("name" := a.name, "value" := a.value))
  implicit val argumentDecoder: Decoder[Argument] =
    Decoder.instance(c => (c.get[String]("name"), c.get[Value]("value")).mapN(Argument(_, _)))

  implicit val pathSegmentEncoder: Encoder[PathSegment] = Encoder.instance {
    case PathSegment.Field(name)  => Json.obj("field" := name)
    case PathSegment.Index(index) => Json.obj("index" := index)
  }
  implicit val pathSegmentDecoder: Decoder[PathSegment] = Decoder.instance { c =>
    c.get[String]("field").map(PathSegment.Field(_)).orElse(c.get[Int]("index").map(PathSegment.Index(_)))
  }

  implicit val fieldErrorEncoder: Encoder[FieldError] =
    Encoder.instance(e => Json.obj("message" := e.message, "path" := e.path.asJson, "code" := e.code))
  implicit val fieldErrorDecoder: Decoder[FieldError] =
    Decoder.instance(c =>
      (c.get[String]("message"), c.get[List[PathSegment]]("path"), c.get[String]("code")).mapN(FieldError(_, _, _))
    )

  implicit val nodeResultEncoder: Encoder[NodeResult] =
    Encoder.instance(r =>
      Json.obj(
        "values" := r.values.map { case (path, json) => Json.obj("path" := path.asJson, "value" := json) },
        "errors" := r.errors.toList.asJson
      )
    )
  implicit val nodeResultDecoder: Decoder[NodeResult] =
    Decoder.instance { c =>
      val values = c
        .get[List[Json]]("values")
        .flatMap(_.traverse { entry =>
          (entry.hcursor.get[List[PathSegment]]("path"), entry.hcursor.get[Json]("value")).mapN(_ -> _)
        })
      (values, c.get[List[FieldError]]("errors")).mapN((v, e) => NodeResult(v, Chain.fromSeq(e)))
    }

  implicit val selectionEncoder: Encoder[Selection]       = Encoder.instance {
    case InlineField(responseKey, field, selections) =>
      Json.obj("kind" := "inline", "responseKey" := responseKey, "field" := field, "selections" := selections.asJson)
    case NodeField(responseKey, field, node)         =>
      Json.obj("kind" := "node", "responseKey" := responseKey, "field" := field, "node" := node.asJson)
  }
  implicit val selectionDecoder: Decoder[Selection]       = Decoder.instance { c =>
    c.get[String]("kind").flatMap {
      case "inline" =>
        (c.get[String]("responseKey"), c.get[String]("field"), c.get[SelectionSet]("selections"))
          .mapN(InlineField(_, _, _))
      case "node"   =>
        (c.get[String]("responseKey"), c.get[String]("field"), c.get[NodeId]("node")).mapN(NodeField(_, _, _))
      case other    => Left(DecodingFailure(s"unknown selection kind $other", c.history))
    }
  }
  implicit val selectionSetEncoder: Encoder[SelectionSet] = Encoder[SelectionSet](ss => ss.selections.asJson)
  implicit val selectionSetDecoder: Decoder[SelectionSet] = Decoder[List[Selection]].map(SelectionSet(_))

  implicit val planNodeEncoder: Encoder[PlanNode] =
    Encoder.instance(n =>
      Json.obj(
        "id"           := n.id,
        "source"       := n.source,
        "field"        := n.field,
        "args"         := n.args,
        "selectionSet" := n.selectionSet,
        "dependencies" := n.dependencies,
        "level"        := n.level
      )
    )
  implicit val planNodeDecoder: Decoder[PlanNode] =
    Decoder.instance(c =>
      (
        c.get[NodeId]("id"),
        c.get[String]("source"),
        c.get[FieldCoordinate]("field"),
        c.get[List[Argument]]("args"),
        c.get[SelectionSet]("selectionSet"),
        c.get[List[NodeId]]("dependencies"),
        c.get[Int]("level")
      ).mapN(PlanNode.apply)
    )

  implicit val planEncoder: Encoder[Plan] =
    Encoder.instance(p => Json.obj("rootType" := p.rootType, "roots" := p.roots, "nodes" := p.nodes))
  implicit val planDecoder: Decoder[Plan] =
    Decoder.instance(c =>
      (c.get[String]("rootType"), c.get[SelectionSet]("roots"), c.get[List[PlanNode]]("nodes")).mapN(Plan(_, _, _))
    )

  implicit val unitEncoder: Encoder[Unit] = Encoder.instance(_ => Json.Null)
  implicit val unitDecoder: Decoder[Unit] = Decoder.instance(_ => Right(()))
}
