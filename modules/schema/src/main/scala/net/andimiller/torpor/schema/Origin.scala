package net.andimiller.torpor.schema

/** Where a schema element came from: a source id plus the position in that source's SDL, if the parser recorded one.
  * Every merge error carries at least one of these, because "field collision" without both locations is not actionable.
  */
final case class Origin(source: String, line: Option[Int] = None, column: Option[Int] = None) {
  def render: String =
    (line, column) match {
      case (Some(l), Some(c)) => s"$source:$l:$c"
      case (Some(l), None)    => s"$source:$l"
      case _                  => source
    }
}

object Origin {
  val synthesized: Origin = Origin("<synthesized>")
}
