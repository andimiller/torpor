package net.andimiller.torpor.schema

import cats.data.NonEmptyChain
import net.andimiller.torpor.core.Value

/** Torpor's own executable document AST, plus the front door: parse, validate, select one operation.
  *
  * The AST is deliberately plain. Fragment expansion, alias handling and selection normalisation belong to the planner
  * (M0 step 3); this layer only guarantees the document is well-formed and provably safe to plan.
  */
enum OperationKind(val label: String) {
  case Query extends OperationKind("query")
  case Mutation extends OperationKind("mutation")
  case Subscription extends OperationKind("subscription")
}

sealed trait Selection {
  def directives: List[Directive]
  def origin: Origin
}

final case class Argument(name: String, value: Value, origin: Origin)

final case class Directive(name: String, args: List[Argument], origin: Origin)

final case class FieldSelection(
    alias: Option[String],
    name: String,
    args: List[Argument],
    directives: List[Directive],
    selections: List[Selection],
    origin: Origin
) extends Selection {
  def responseKey: String = alias.getOrElse(name)
}

final case class FragmentSpread(name: String, directives: List[Directive], origin: Origin) extends Selection

final case class InlineFragment(
    typeCondition: Option[String],
    directives: List[Directive],
    selections: List[Selection],
    origin: Origin
) extends Selection

final case class VariableDefinition(name: String, tpe: TypeRef, default: Option[Value], origin: Origin)

final case class OperationDefinition(
    kind: OperationKind,
    name: Option[String],
    variables: List[VariableDefinition],
    directives: List[Directive],
    selections: List[Selection],
    origin: Origin
)

final case class FragmentDefinition(
    name: String,
    typeCondition: String,
    directives: List[Directive],
    selections: List[Selection],
    origin: Origin
)

final case class ExecutableDocument(operations: List[OperationDefinition], fragments: List[FragmentDefinition]) {
  def fragment(name: String): Option[FragmentDefinition] = fragments.find(_.name == name)
}

/** A validated document plus the operation the caller asked for. The planner needs the fragments for expansion, which
  * is why it takes this rather than a bare `OperationDefinition`.
  */
final case class Validated(document: ExecutableDocument, operation: OperationDefinition)

object Executable {

  /** Parse and validate `query` against `schema`, then select the operation to run.
    *
    * The whole document is validated, per the spec; `operationName` only chooses which operation the caller gets.
    */
  def validate(
      schema: Schema,
      query: String,
      operationName: Option[String] = None,
      source: String = "query"
  ): Either[NonEmptyChain[ValidationError], Validated] =
    for {
      document <- parse(schema, query, source)
      selected <- select(document, operationName, source)
    } yield Validated(document, selected)

  /** Parse and validate without selecting an operation. Used by the conformance suite. */
  def parse(
      schema: Schema,
      query: String,
      source: String = "query"
  ): Either[NonEmptyChain[ValidationError], ExecutableDocument] =
    SangriaBridge.parseExecutable(source, query).flatMap { document =>
      NonEmptyChain.fromSeq(Validator.validate(schema, document)) match {
        case Some(errors) => Left(errors)
        case None         => Right(document)
      }
    }

  private def select(
      document: ExecutableDocument,
      operationName: Option[String],
      source: String
  ): Either[NonEmptyChain[ValidationError], OperationDefinition] =
    (operationName, document.operations) match {
      case (Some(name), ops) =>
        ops.find(_.name.contains(name)) match {
          case Some(op) => Right(op)
          case None     =>
            Left(
              error(
                ValidationRule.OperationSelection,
                s"no operation named '$name' in the document",
                origin(document, source)
              )
            )
        }
      case (None, op :: Nil) => Right(op)
      case (None, Nil)       =>
        Left(error(ValidationRule.OperationSelection, "the document contains no operations", Origin(source)))
      case (None, ops)       =>
        Left(
          error(
            ValidationRule.OperationSelection,
            s"the document contains ${ops.size} operations, so an operationName is required",
            ops.head.origin
          )
        )
    }

  private def origin(document: ExecutableDocument, source: String): Origin =
    document.operations.headOption.map(_.origin).getOrElse(Origin(source))

  private def error(rule: ValidationRule, detail: String, at: Origin): NonEmptyChain[ValidationError] =
    NonEmptyChain.one(ValidationError(rule, detail, at))
}
