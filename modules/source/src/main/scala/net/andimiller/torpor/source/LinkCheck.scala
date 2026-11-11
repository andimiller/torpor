package net.andimiller.torpor.source

import scala.quoted.*

/** The compile-time half of typed assembly: every keyed resolver must be able to decode what every producer of its
  * parent type produces.
  *
  * For each `Consumes[P, T, N]` (the keyed field `T.N` decodes parents as `P`) and each `Produces[T, B, …]` (some
  * resolver produces `T` values as `B`), every field of `P` must exist on `B` with a conforming type. A field of `P`
  * that is an `Option` may be missing from `B`, since it decodes as `None`.
  *
  * This compares Scala case-class fields, not JSON: it assumes producer and consumer codecs share a naming
  * configuration, and it cannot see into hand-written codecs or non-case-class types, which it reports and skips.
  */
private[source] object LinkCheck {

  inline def verify[Sources <: Tuple]: Unit = ${ verifyImpl[Sources] }

  private def verifyImpl[Sources <: Tuple: Type](using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    // `Long`, not `scala.Long`, in messages
    extension (tpe: TypeRepr) def name: String = tpe.show(using Printer.TypeReprShortCode)

    final case class Producer(graphqlType: String, scala: TypeRepr, at: String, source: String)
    final case class Consumer(graphqlType: String, scala: TypeRepr, at: String, source: String)

    def elements(tuple: TypeRepr): List[TypeRepr] = tuple.simplified.dealias.asType match {
      case '[EmptyTuple] => Nil
      case '[h *: t]     => TypeRepr.of[h] :: elements(TypeRepr.of[t])
      case _             => report.errorAndAbort(s"not a tuple: ${tuple.show}")
    }

    def literal(tpe: TypeRepr): String = tpe.dealias match {
      case ConstantType(StringConstant(s)) => s
      case other                           => report.errorAndAbort(s"expected a literal string type, found ${other.show}")
    }

    // a root returning Option[List[EsiCharacter]] produces EsiCharacter values
    def unwrap(tpe: TypeRepr): TypeRepr = tpe.dealias.asType match {
      case '[Option[a]] => unwrap(TypeRepr.of[a])
      case '[List[a]]   => unwrap(TypeRepr.of[a])
      case '[Seq[a]]    => unwrap(TypeRepr.of[a])
      case _            => tpe
    }

    def caseFields(tpe: TypeRepr): Option[List[(String, TypeRepr)]] = {
      val symbol = tpe.typeSymbol
      if (symbol.flags.is(Flags.Case)) Some(symbol.caseFields.map(field => field.name -> tpe.memberType(field)))
      else None
    }

    val facts = elements(TypeRepr.of[Sources]).flatMap { pair =>
      elements(pair) match {
        case List(id, _, sourceFacts) => elements(sourceFacts).map(fact => literal(id) -> fact)
        case other                    => report.errorAndAbort(s"expected an (id, required, facts) triple, found ${pair.show}")
      }
    }

    val producers = facts.collect {
      case (source, fact) if fact <:< TypeRepr.of[Produces[?, ?, ?, ?]] =>
        fact.dealias match {
          case AppliedType(_, List(g, b, parent, field)) =>
            Producer(literal(g), unwrap(b), s"${literal(parent)}.${literal(field)}", source)
          case other                                     => report.errorAndAbort(s"malformed fact ${other.show}")
        }
    }

    val consumers = facts.collect {
      case (source, fact) if fact <:< TypeRepr.of[Consumes[?, ?, ?]] =>
        fact.dealias match {
          case AppliedType(_, List(p, parent, field)) =>
            Consumer(literal(parent), p, s"${literal(parent)}.${literal(field)}", source)
          case other                                  => report.errorAndAbort(s"malformed fact ${other.show}")
        }
    }

    val problems = for {
      consumer <- consumers
      needs    <- caseFields(consumer.scala).toList.flatten match {
                    case Nil    =>
                      report.warning(
                        s"${consumer.at} (${consumer.source}) decodes its parent as ${consumer.scala.name}, which is not a case class; its link is not checked"
                      )
                      Nil
                    case fields => List(fields)
                  }
      producer <- producers.filter(_.graphqlType == consumer.graphqlType)
      problem  <- caseFields(producer.scala) match {
                    case None      =>
                      report.warning(
                        s"${producer.at} (${producer.source}) produces ${producer.graphqlType} as ${producer.scala.name}, which is not a case class; its link to ${consumer.at} is not checked"
                      )
                      Nil
                    case Some(has) =>
                      needs.flatMap { case (name, needed) =>
                        has.find(_._1 == name) match {
                          case Some((_, actual)) if actual <:< needed      => None
                          case Some((_, actual))                           =>
                            Some(
                              s"${consumer.at} (${consumer.source}) reads `$name: ${needed.name}` from its parent, but ${producer.at} (${producer.source}) produces ${producer.graphqlType} as ${producer.scala.name}, where `$name` is ${actual.name}"
                            )
                          case None if needed <:< TypeRepr.of[Option[Any]] => None
                          case None                                        =>
                            Some(
                              s"${consumer.at} (${consumer.source}) reads `$name: ${needed.name}` from its parent, but ${producer.at} (${producer.source}) produces ${producer.graphqlType} as ${producer.scala.name}, which has no `$name`"
                            )
                        }
                      }
                  }
    } yield problem

    if (problems.nonEmpty) report.errorAndAbort(problems.distinct.mkString("sources do not link:\n  ", "\n  ", ""))
    '{ () }
  }
}
