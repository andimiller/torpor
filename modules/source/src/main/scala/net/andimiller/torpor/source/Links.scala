package net.andimiller.torpor.source

import net.andimiller.torpor.core.FieldCoordinate

import scala.compiletime.{constValue, erasedValue, error, summonFrom}
import scala.compiletime.ops.any.ToString
import scala.compiletime.ops.string.+
import scala.deriving.Mirror

/** How each keyed field reads the values that other sources produce, worked out when `Torpor[F](…)` compiles.
  *
  * For each `Consumes[P, T, N]` (the keyed field `T.N` reads its parent as `P`) and each `Produces[T, B, …]` (some
  * resolver produces `T` values as `B`), the parent must be readable as `P` without JSON:
  *
  *   - if `B <: P`, the produced value is the parent as it is;
  *   - otherwise both must be case classes, and every field of `P` must be a field of `B` with a conforming type. A
  *     field of `P` that is an `Option` may be missing from `B`, and reads as `None`.
  *
  * Anything else does not compile. Only inline matches and `Mirror`s do this, with no macro.
  */
private[source] object Links {

  /** For a keyed field: for each producer of its parent type, by the producer's coordinate, how to read a produced
    * value as the keyed field's parent type.
    */
  type ByProducer = Map[FieldCoordinate, Any => Any]

  /** Each source's facts, as `Torpor.SourcesOf` lays them out, in one tuple. */
  type AllFacts[Sources] <: Tuple = Sources match {
    case (?, ?, facts) *: rest => Append[facts, AllFacts[rest]]
    case EmptyTuple            => EmptyTuple
  }

  type Append[A, B <: Tuple] <: Tuple = A match {
    case head *: tail => head *: Append[tail, B]
    case EmptyTuple   => B
  }

  /** For each keyed field, by its coordinate. */
  inline def of[Sources <: Tuple]: Map[FieldCoordinate, ByProducer] =
    consumers[AllFacts[Sources], AllFacts[Sources]].toMap

  private inline def consumers[Remaining <: Tuple, Facts <: Tuple]: List[(FieldCoordinate, ByProducer)] =
    inline erasedValue[Remaining] match {
      case _: EmptyTuple                           => Nil
      case _: (Consumes[p, parent, field] *: rest) =>
        (coordinate[parent, field] -> producers[p, parent, ToString[parent] + "." + ToString[field], Facts].toMap) ::
          consumers[rest, Facts]
      case _: (? *: rest)                          => consumers[rest, Facts]
    }

  private inline def producers[P, Parent, Consumer <: String, Remaining <: Tuple]: List[(FieldCoordinate, Any => Any)] =
    inline erasedValue[Remaining] match {
      case _: EmptyTuple                                   => Nil
      case _: (Produces[Parent, b, parent, field] *: rest) =>
        (coordinate[parent, field] -> link[
          b,
          P,
          Consumer,
          ToString[parent] + "." + ToString[field],
          ToString[Parent]
        ]) ::
          producers[P, Parent, Consumer, rest]
      case _: (? *: rest)                                  => producers[P, Parent, Consumer, rest]
    }

  // a root returning Option[List[EsiCharacter]] produces EsiCharacter values; the executor unwraps them the same way
  private inline def link[B, P, Consumer <: String, Producer <: String, G <: String]: Any => Any =
    inline erasedValue[B] match {
      case _: Option[b]   => link[b, P, Consumer, Producer, G]
      case _: Iterable[b] => link[b, P, Consumer, Producer, G]
      case _              =>
        summonFrom {
          case _: (B <:< P) => (value: Any) => value
          case _            => project[B, P, Consumer, Producer, G]
        }
    }

  private inline def project[B, P, Consumer <: String, Producer <: String, G <: String]: Any => Any =
    summonFrom {
      case consumer: Mirror.ProductOf[P] =>
        summonFrom {
          case producer: Mirror.ProductOf[B] =>
            val indices = fields[
              consumer.MirroredElemLabels,
              consumer.MirroredElemTypes,
              producer.MirroredElemLabels,
              producer.MirroredElemTypes,
              Consumer,
              Producer,
              G
            ].toArray
            (value: Any) => {
              val product = value.asInstanceOf[Product]
              consumer.fromProduct(
                Tuple.fromArray(indices.map(index => if (index < 0) None else product.productElement(index)))
              )
            }
          case _                             =>
            error(
              constValue[
                Consumer + " reads its parent as a case class, but " + Producer + " produces " + G +
                  " as a type that is neither a case class nor a subtype of it"
              ]
            )
        }
      case _                             =>
        error(
          constValue[
            Consumer + " reads its parent as a type that is neither a case class nor a supertype of what " +
              Producer + " produces as " + G
          ]
        )
    }

  /** For each field of the consumer's case class, its index in the producer's, or -1 for a missing `Option`. */
  private inline def fields[
      Labels <: Tuple,
      Types <: Tuple,
      ProducerLabels <: Tuple,
      ProducerTypes <: Tuple,
      Consumer <: String,
      Producer <: String,
      G <: String
  ]: List[Int] =
    inline erasedValue[(Labels, Types)] match {
      case _: (EmptyTuple, EmptyTuple)        => Nil
      case _: (label *: labels, tpe *: types) =>
        indexOf[label, tpe, ProducerLabels, ProducerTypes, Consumer, Producer, G](0) ::
          fields[labels, types, ProducerLabels, ProducerTypes, Consumer, Producer, G]
    }

  private inline def indexOf[
      Label,
      Type,
      Labels <: Tuple,
      Types <: Tuple,
      Consumer <: String,
      Producer <: String,
      G <: String
  ](index: Int): Int =
    inline erasedValue[(Labels, Types)] match {
      case _: (Label *: ?, actual *: ?) =>
        inline erasedValue[actual] match {
          case _: Type => index
          case _       =>
            error(
              constValue[
                Consumer + " reads `" + ToString[Label] + "` from its parent, but " + Producer + " produces " +
                  G + " with `" + ToString[Label] + "` of another type"
              ]
            )
        }
      case _: (? *: labels, ? *: types) => indexOf[Label, Type, labels, types, Consumer, Producer, G](index + 1)
      case _: (EmptyTuple, EmptyTuple)  =>
        inline erasedValue[Type] match {
          case _: Option[?] => -1
          case _            =>
            error(
              constValue[
                Consumer + " reads `" + ToString[Label] + "` from its parent, but " + Producer + " produces " +
                  G + " without it"
              ]
            )
        }
    }

  private inline def coordinate[T, N]: FieldCoordinate =
    FieldCoordinate(constValue[T].toString, constValue[N].toString)
}
