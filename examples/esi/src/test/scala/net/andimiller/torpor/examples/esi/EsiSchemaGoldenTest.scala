package net.andimiller.torpor.examples.esi

import java.nio.file.Paths

import net.andimiller.torpor.core.Sdl
import net.andimiller.torpor.testkit.GoldenGraphqlsTest

/** The merged contract as one file: `src/test/resources/full.graphqls`. Nothing to do when it matches; a diff when it
  * does not, and a freshly written file when it is absent.
  */
final class EsiSchemaGoldenTest extends GoldenGraphqlsTest {

  def schemas: List[Sdl[?]] = List(EsiGraphQL.characters, EsiGraphQL.universe, EsiGraphQL.routes)

  override def golden = Paths.get("examples/esi/src/test/resources/full.graphqls")
}
