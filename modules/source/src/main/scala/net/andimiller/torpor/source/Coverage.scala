package net.andimiller.torpor.source

import scala.compiletime.{constValue, error}
import scala.compiletime.ops.any.ToString
import scala.compiletime.ops.string.+

/** Every source must resolve exactly the fields its SDL requires, which is checked when the sources are assembled.
  *
  * This is all types: a source's `Required` is a tuple of `"Type.field"` literals that the `schema` macro read from its
  * SDL, and each of its resolvers records a `Produces` fact naming the coordinate it serves. The match types below
  * compare the two and spell out any difference as a literal string type, which `verify` raises as the compile error.
  */
private[source] object Coverage {

  /** The `"Type.field"` coordinates a source's resolvers serve: one per `Produces` fact. */
  type Served[Facts] <: Tuple = Facts match {
    case Produces[?, ?, parent, field] *: rest => (ToString[parent] + "." + ToString[field]) *: Served[rest]
    case ? *: rest                             => Served[rest]
    case EmptyTuple                            => EmptyTuple
  }

  type Contains[T, X] <: Boolean = T match {
    case X *: ?     => true
    case ? *: rest  => Contains[rest, X]
    case EmptyTuple => false
  }

  /** The elements of `A` that are not in `B`. */
  type Without[A, B] <: Tuple = A match {
    case head *: tail =>
      Contains[B, head] match {
        case true  => Without[tail, B]
        case false => head *: Without[tail, B]
      }
    case EmptyTuple   => EmptyTuple
  }

  type Join[T, Separator] <: String = T match {
    case EmptyTuple         => ""
    case head *: EmptyTuple => ToString[head]
    case head *: tail       => ToString[head] + ToString[Separator] + Join[tail, Separator]
  }

  type Missing[Required, Facts]    = Without[Required, Served[Facts]]
  type Unexpected[Required, Facts] = Without[Served[Facts], Required]

  /** What is wrong with one source, as a tuple of messages: empty when it resolves exactly what its SDL requires. */
  type SourceProblems[Id, Required, Facts] <: Tuple = (Missing[Required, Facts], Unexpected[Required, Facts]) match {
    case (EmptyTuple, EmptyTuple) => EmptyTuple
    case (missing, EmptyTuple)    =>
      ("source '" + ToString[Id] + "' has no resolver for " + Join[missing, ", "]) *: EmptyTuple
    case (EmptyTuple, unexpected) =>
      ("source '" + ToString[Id] + "' resolves fields its SDL does not require: " + Join[
        unexpected,
        ", "
      ]) *: EmptyTuple
    case (missing, unexpected)    =>
      ("source '" + ToString[Id] + "' has no resolver for " + Join[missing, ", "]) *:
        ("source '" + ToString[Id] + "' resolves fields its SDL does not require: " + Join[unexpected, ", "]) *:
        EmptyTuple
  }

  /** Every source's problems, for `Sources` as `Torpor.SourcesOf` lays them out: `(id, required, facts)` triples. */
  type Problems[Sources] <: Tuple = Sources match {
    case (id, required, facts) *: rest => Tuple.Concat[SourceProblems[id, required, facts], Problems[rest]]
    case EmptyTuple                    => EmptyTuple
  }

  type Report[Sources] <: String = Problems[Sources] match {
    case EmptyTuple   => ""
    case head *: tail => "sources do not cover their SDL:\n  " + Join[head *: tail, "\n  "]
  }

  inline def verify[Sources <: Tuple]: Unit =
    inline constValue[Report[Sources]] match {
      case "" => ()
      case _  => error(constValue[Report[Sources]])
    }
}
