package net.andimiller.torpor.schema

import net.andimiller.torpor.core.Value

import java.security.MessageDigest

/** A stable hash of the merged schema, stored in every snapshot.
  *
  * Computed from the parsed, merged model rather than the source text, so formatting or fragment order does not change
  * it, and a resumed run is refused when the schema it was planned against is gone.
  */
object SchemaFingerprint {

  def of(schema: Schema): String = {
    val canonical = new StringBuilder

    schema.types.toList.sortBy(_._1).foreach { case (name, tpe) =>
      canonical.append(name).append(':').append(tpe.kind).append('|')
      tpe.fields.sortBy(_.name).foreach { field =>
        canonical.append(field.name).append('(')
        field.args
          .sortBy(_.name)
          .foreach(arg =>
            canonical
              .append(arg.name)
              .append(':')
              .append(render(arg.tpe))
              .append(arg.default.fold("")(value => "=" + renderValue(value)))
              .append(',')
          )
        canonical.append("):").append(render(field.tpe)).append(';')
      }
      canonical.append("interfaces=").append(tpe.interfaces.sorted.mkString(","))
      canonical.append(";members=").append(tpe.unionMembers.sorted.mkString(","))
      canonical.append(";values=").append(tpe.enumValues.map(_.name).sorted.mkString(","))
      canonical.append('\n')
    }

    canonical.append("query=").append(schema.query.getOrElse(""))
    canonical.append(";mutation=").append(schema.mutation.getOrElse(""))
    canonical.append(";subscription=").append(schema.subscription.getOrElse(""))

    val digest = MessageDigest.getInstance("SHA-256").digest(canonical.toString.getBytes("UTF-8"))
    digest.map(b => f"$b%02x").mkString
  }

  private def renderValue(value: Value): String =
    value.toJson.noSpaces

  private def render(tpe: TypeRef): String =
    tpe match {
      case TypeRef.Named(name) => name
      case TypeRef.ListOf(of)  => s"[${render(of)}]"
      case TypeRef.NonNull(of) => s"${render(of)}!"
    }
}
