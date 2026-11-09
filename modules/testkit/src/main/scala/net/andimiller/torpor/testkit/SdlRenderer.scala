package net.andimiller.torpor.testkit

import net.andimiller.torpor.schema.{FieldDef, InputValueDef, Schema, TypeDef, TypeKind, TypeRef}

/** The merged schema as SDL, so the contract can be reviewed as one file.
  *
  * Types are sorted by name; fields keep the order the sources declared them, which is the order clients see. Defaults
  * are not printed because the schema model does not keep them.
  */
object SdlRenderer {

  def render(schema: Schema): String =
    schema.types.values.toList.sortBy(_.name).map(renderType).mkString("\n\n")

  private def renderType(tpe: TypeDef): String = {
    val description = tpe.description.toList.map(text)
    val body        = tpe.kind match {
      case TypeKind.ScalarType                          => List(s"scalar ${tpe.name}")
      case TypeKind.UnionType                           => List(s"union ${tpe.name} = ${tpe.unionMembers.mkString(" | ")}")
      case TypeKind.EnumType                            => (s"enum ${tpe.name} {" :: tpe.enumValues.map(value => s"  $value")) :+ "}"
      case TypeKind.InputObjectType                     =>
        (s"input ${tpe.name} {" :: tpe.fields.map(renderField)) :+ "}"
      case TypeKind.ObjectType | TypeKind.InterfaceType =>
        val keyword    = if (tpe.kind == TypeKind.ObjectType) "type" else "interface"
        val implements = if (tpe.interfaces.isEmpty) "" else tpe.interfaces.mkString(" implements ", " & ", "")
        (s"$keyword ${tpe.name}$implements {" :: tpe.fields.map(renderField)) :+ "}"
    }
    (description ++ body).mkString("\n")
  }

  private def renderField(field: FieldDef): String = {
    val args = if (field.args.isEmpty) "" else field.args.map(renderArgument).mkString("(", ", ", ")")
    field.description.fold("")(description => text(description) + "\n") +
      s"  ${field.name}$args: ${renderRef(field.tpe)}"
  }

  private def renderArgument(argument: InputValueDef): String = s"${argument.name}: ${renderRef(argument.tpe)}"

  private def renderRef(ref: TypeRef): String = ref match {
    case TypeRef.Named(name) => name
    case TypeRef.ListOf(of)  => s"[${renderRef(of)}]"
    case TypeRef.NonNull(of) => s"${renderRef(of)}!"
  }

  private def text(description: String): String =
    if (description.contains("\"\"\"")) s"\"${description.replace("\"", "\\\"")}\"" else s"\"\"\"$description\"\"\""
}
