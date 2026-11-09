package net.andimiller.torpor.core

import io.circe.Json

/** GraphQL input and literal values, in a form that serialises.
  *
  * Lives in `torpor-core` rather than `torpor-schema` because plans carry argument values, and the plan has to survive
  * a snapshot round trip; nothing here may reference the parser.
  */
enum Value {
  case IntValue(value: BigInt)
  case FloatValue(value: BigDecimal)
  case StringValue(value: String)
  case BooleanValue(value: Boolean)
  case EnumValue(value: String)
  case NullValue
  case ListValue(values: List[Value])
  case ObjectValue(fields: List[(String, Value)])
  case VariableValue(name: String)

  def toJson: Json = this match {
    case IntValue(value)     => Json.fromBigInt(value)
    case FloatValue(value)   => Json.fromBigDecimal(value)
    case StringValue(value)  => Json.fromString(value)
    case BooleanValue(value) => Json.fromBoolean(value)
    case EnumValue(value)    => Json.fromString(value)
    case NullValue           => Json.Null
    case ListValue(values)   => Json.fromValues(values.map(_.toJson))
    case ObjectValue(fields) => Json.obj(fields.map { case (name, value) => name -> value.toJson }: _*)
    // The planner substitutes variables, so this is defensive only.
    case VariableValue(_)    => Json.Null
  }
}

object Value {

  /** Read a client-supplied variable value. Numbers stay numbers, strings stay strings: enum-vs-string is a coercion
    * question the planner does not answer (5.6.1 is not implemented), so nothing here guesses.
    */
  def fromJson(json: Json): Value =
    json.fold(
      jsonNull = Value.NullValue,
      jsonBoolean = Value.BooleanValue.apply,
      jsonNumber = number =>
        number.toBigInt match {
          case Some(int) => Value.IntValue(int)
          case None      => Value.FloatValue(number.toBigDecimal.getOrElse(BigDecimal(0)))
        },
      jsonString = Value.StringValue.apply,
      jsonArray = values => Value.ListValue(values.toList.map(fromJson)),
      jsonObject = obj => Value.ObjectValue(obj.toList.map { case (name, value) => name -> fromJson(value) })
    )
}
