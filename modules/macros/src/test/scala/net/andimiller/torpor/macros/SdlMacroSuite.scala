package net.andimiller.torpor.macros

class SdlMacroSuite extends munit.FunSuite {

  test("a valid SDL literal compiles and is unchanged") {
    val text = sdl"""type Query { a: Int }"""
    assertEquals(text, "type Query { a: Int }")
  }

  test("a syntax error is a compile error, reported as SDL") {
    val errors = compileErrors("""sdl"type Query {" """)
    assert(errors.contains("cannot parse SDL"), errors)
  }

  test("a client query is rejected at compile time, like startup would") {
    val errors = compileErrors("""sdl"{ a }" """)
    assert(errors.contains("found a query operation"), errors)
  }

  test("interpolation is refused: it would move parsing back to runtime") {
    val errors = compileErrors("""val x = 1; sdl"type Query { a: Int } $x" """)
    assert(errors.nonEmpty)
  }
}
