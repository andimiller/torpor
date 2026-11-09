package net.andimiller.torpor.testkit

import java.nio.file.{Files, Path, Paths}

import net.andimiller.torpor.core.Sdl
import net.andimiller.torpor.schema.{Schema, SdlFragment}

/** The merged SDL as one golden file: the contract a client team reads, checked on every run.
  *
  * {{{
  * final class SchemaGoldenTest extends GoldenGraphqlsTest {
  *   def schemas = List(EsiGraphQL.characters, EsiGraphQL.universe, EsiGraphQL.routes)
  *   override def golden = Paths.get("examples/esi/src/test/resources/full.graphqls")
  * }
  * }}}
  *
  * A missing file is written rather than failed, so the first run of a new example produces the file to review.
  */
abstract class GoldenGraphqlsTest extends munit.FunSuite {

  /** The `schema"""…"""` values, in any order: they are merged before rendering, exactly as at startup. */
  def schemas: List[Sdl]

  /** Where the merged contract is kept, relative to the working directory: under sbt, the build root. */
  def golden: Path = Paths.get("src/test/resources/schema.graphqls")

  test(s"the merged schema matches $golden") {
    GoldenGraphqls.assertMatches(golden, schemas.map(_.text))
  }
}

object GoldenGraphqls extends munit.Assertions {

  /** Merge the SDL fragments into one schema and render it as a document, newline-terminated. */
  def render(fragments: List[String]): String = {
    val mergeable = fragments.zipWithIndex.map { case (sdl, index) => SdlFragment(s"source-$index", sdl) }
    val merged    = Schema
      .merge(mergeable)
      .fold(
        errors => fail(errors.toChain.toList.map(_.render).mkString("\n")),
        identity
      )
    SdlRenderer.render(merged) + "\n"
  }

  /** Write `file` when it does not exist yet; otherwise fail with a diff if the merged schema no longer matches it. */
  def assertMatches(file: Path, fragments: List[String]): Unit =
    if (Files.exists(file)) assertNoDiff(render(fragments), Files.readString(file), clue = s"$file")
    else {
      Option(file.getParent).foreach(Files.createDirectories(_))
      Files.writeString(file, render(fragments))
      println(s"created $file: review it, then commit it")
    }
}
