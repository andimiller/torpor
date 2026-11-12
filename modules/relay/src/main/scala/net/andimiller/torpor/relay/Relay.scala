package net.andimiller.torpor.relay

import io.circe.Decoder

/** Relay pagination over an already-ordered list.
  *
  * torpor does not need to know about Relay: `first`/`after`/`last`/`before` are ordinary field arguments, and this
  * module is the one piece every adopter would otherwise write themselves. Cursors are opaque strings the server
  * generates; slicing only compares them for equality.
  */
final case class PageArgs(
    first: Option[Int] = None,
    after: Option[String] = None,
    last: Option[Int] = None,
    before: Option[String] = None
)

object PageArgs {

  /** So a root or keyed resolver can take `PageArgs` directly with `.args[PageArgs]`. */
  given Decoder[PageArgs] = Decoder.instance { cursor =>
    for {
      first  <- cursor.get[Option[Int]]("first")
      after  <- cursor.get[Option[String]]("after")
      last   <- cursor.get[Option[Int]]("last")
      before <- cursor.get[Option[String]]("before")
    } yield PageArgs(first, after, last, before)
  }
}

final case class Edge[A](cursor: String, node: A)

final case class PageInfo(
    hasNextPage: Boolean,
    hasPreviousPage: Boolean,
    startCursor: Option[String],
    endCursor: Option[String]
)

final case class Connection[A](edges: List[Edge[A]], pageInfo: PageInfo)

object Connection {

  /** Slice `items` (the full ordered set, or at least everything between the requested cursors) and describe the page.
    *
    * An unknown cursor yields an empty page: the server cannot prove what lies beyond a cursor it never issued. When
    * both `first` and `last` are given, `first` is applied first, then `last`, per the spec's order.
    */
  def from[A](items: List[A], args: PageArgs, cursor: A => String): Connection[A] = {
    val start = args.after match {
      case None    => Some(0)
      case Some(c) =>
        items.indexWhere(cursor(_) == c) match {
          case -1 => None
          case i  => Some(i + 1)
        }
    }

    val end = args.before match {
      case None    => Some(items.size)
      case Some(c) =>
        items.indexWhere(cursor(_) == c) match {
          case -1 => None
          case i  => Some(i)
        }
    }

    val window = (start, end) match {
      case (Some(from), Some(to)) => items.slice(from, to)
      case _                      => Nil
    }

    val firstPage = args.first.fold(window)(n => window.take(Math.max(0, n)))
    val page      = args.last.fold(firstPage)(n => firstPage.takeRight(Math.max(0, n)))

    val edges = page.map(item => Edge(cursor(item), item))

    Connection(
      edges,
      PageInfo(
        hasNextPage = args.first.exists(n => window.size > n),
        hasPreviousPage = args.last.exists(n => window.size > n),
        startCursor = edges.headOption.map(_.cursor),
        endCursor = edges.lastOption.map(_.cursor)
      )
    )
  }
}
