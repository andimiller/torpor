package net.andimiller.torpor

import net.andimiller.torpor.core.Sdl

import scala.quoted.*

package object macros {

  extension (inline sc: StringContext) inline def sdl(inline args: Any*): String = ${ SdlMacro.impl('sc, 'args) }

  /** SDL as a value: the text to merge, plus its field coordinates, both validated at compile time. */
  extension (inline sc: StringContext)
    transparent inline def schema(inline args: Any*): Sdl[?] = ${
      SchemaMacro.impl('sc, 'args)
    }
}
