package net.andimiller.torpor.relay

class RelaySuite extends munit.FunSuite {

  private val items                         = List("a", "b", "c", "d")
  private def cursor(value: String): String = s"cursor:$value"

  test("first returns the head of the list") {
    val page = Connection.from(items, PageArgs(first = Some(2)), cursor)
    assertEquals(page.edges.map(_.node), List("a", "b"))
    assertEquals(page.pageInfo.hasNextPage, true)
    assertEquals(page.pageInfo.hasPreviousPage, false)
    assertEquals(page.pageInfo.startCursor, Some("cursor:a"))
    assertEquals(page.pageInfo.endCursor, Some("cursor:b"))
  }

  test("after skips the cursor itself") {
    val page = Connection.from(items, PageArgs(first = Some(2), after = Some("cursor:b")), cursor)
    assertEquals(page.edges.map(_.node), List("c", "d"))
    assertEquals(page.pageInfo.hasNextPage, false)
  }

  test("last returns the tail of the list and reports hasPreviousPage") {
    val page = Connection.from(items, PageArgs(last = Some(2)), cursor)
    assertEquals(page.edges.map(_.node), List("c", "d"))
    assertEquals(page.pageInfo.hasNextPage, false)
    assertEquals(page.pageInfo.hasPreviousPage, true)
  }

  test("last before a cursor slices backwards") {
    val page = Connection.from(items, PageArgs(last = Some(2), before = Some("cursor:d")), cursor)
    assertEquals(page.edges.map(_.node), List("b", "c"))
    assertEquals(page.pageInfo.hasPreviousPage, true)
    assertEquals(page.pageInfo.hasNextPage, false)
  }

  test("an unknown cursor yields an empty page rather than guessing") {
    val page = Connection.from(items, PageArgs(first = Some(2), after = Some("cursor:nope")), cursor)
    assertEquals(page.edges, Nil)
    assertEquals(page.pageInfo.startCursor, None)
  }

  test("an empty list is a valid page") {
    val page = Connection.from(Nil, PageArgs(first = Some(2)), cursor)
    assertEquals(page.edges, Nil)
    assertEquals(page.pageInfo.hasNextPage, false)
    assertEquals(page.pageInfo.hasPreviousPage, false)
  }

  test("a negative count is clamped to zero, not an exception") {
    val page = Connection.from(items, PageArgs(first = Some(-1)), cursor)
    assertEquals(page.edges, Nil)
    assertEquals(page.pageInfo.hasNextPage, true)
  }
}
