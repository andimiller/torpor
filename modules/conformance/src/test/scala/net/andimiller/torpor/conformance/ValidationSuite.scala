package net.andimiller.torpor.conformance

import cats.data.NonEmptyChain
import net.andimiller.torpor.schema.*
import net.andimiller.torpor.schema.ValidationRule.*

/** Validation conformance for the M0 subset.
  *
  * The fixture queries follow the examples of section 5 of the GraphQL specification, cross-checked against grackle's
  * conformance suites (`modules/core/src/test/scala/conformance/Validation*Suite.scala`, Apache 2.0). Each case names
  * the rule it targets, so a rule that silently stops firing shows up as a failure rather than a passing document.
  */
class ValidationSuite extends munit.FunSuite {

  /** The schema of the specification's section 5 examples, minus the parts the M0 subset does not use. */
  private val specSdl = SdlFragment(
    "spec",
    """
      |type Query {
      |  dog: Dog
      |  pet: Pet
      |  catOrDog: CatOrDog
      |  human: Human
      |  search(term: String!): [Pet!]!
      |  withDefault(x: Int! = 1): Int
      |  version: String
      |}
      |
      |type Mutation {
      |  addPet(input: PetInput!): Pet
      |}
      |
      |enum DogCommand { SIT DOWN HEEL }
      |
      |interface Pet { name: String! }
      |
      |type Dog implements Pet {
      |  name: String!
      |  nickname: String
      |  barkVolume: Int
      |  doesKnowCommand(dogCommand: DogCommand!): Boolean!
      |  owner: Human
      |}
      |
      |type Cat implements Pet {
      |  name: String!
      |  nickname: String
      |  meowVolume: Int
      |}
      |
      |type Human { name: String! pets: [Pet!] }
      |
      |union CatOrDog = Cat | Dog
      |
      |input PetInput { name: String! nickname: String barkVolume: Int }
      |""".stripMargin
  )

  private val petShop = List(
    SdlFragment(
      "pets",
      """
        |enum Species { DOG CAT RABBIT }
        |type Pet { id: ID! name: String! species: Species! }
        |extend type Query { pets(species: Species, first: Int = 20): [Pet!]! }
        |""".stripMargin
    ),
    SdlFragment(
      "owners",
      """
        |type Owner { id: ID! name: String! email: String! }
        |extend type Pet { owner: Owner! }
        |""".stripMargin
    )
  )

  private def schemaOf(fragments: List[SdlFragment]): Schema =
    Schema.merge(fragments) match {
      case Right(schema) => schema
      case Left(errs)    => fail(errs.toChain.toList.map(_.render).mkString("; "))
    }

  private lazy val spec  = schemaOf(List(specSdl))
  private lazy val shop  = schemaOf(petShop)
  private lazy val plain = schemaOf(List(SdlFragment("plain", "type Query { a: Int }")))

  private def errorList(errors: NonEmptyChain[ValidationError]): String =
    errors.toChain.toList.map(_.render).mkString("\n")

  private def valid(name: String, query: String, schema: Schema = spec): Unit =
    test(s"valid: $name") {
      Executable.parse(schema, query) match {
        case Right(_)     => ()
        case Left(errors) => fail(errorList(errors))
      }
    }

  private def invalid(
      name: String,
      query: String,
      rule: ValidationRule,
      contains: String,
      schema: Schema = spec
  ): Unit =
    test(s"invalid: $name") {
      Executable.parse(schema, query) match {
        case Right(_)     =>
          fail(s"expected ${rule.label}, but the query was accepted")
        case Left(errors) =>
          val matched = errors.toChain.exists(e => e.rule == rule && e.render.contains(contains))
          assert(matched, s"expected ${rule.label} containing '$contains', got:\n${errorList(errors)}")
      }
    }

  // 5.1.1 Executable Definitions

  invalid(
    "a type system definition in a query document",
    "type Foo { a: Int }",
    ExecutableDefinitions,
    "found: object type"
  )

  // 5.2.1.1 Operation Name Uniqueness

  invalid(
    "two operations with the same name",
    "query A { version } query A { version }",
    OperationNameUniqueness,
    "operation 'A' is defined 2 times"
  )

  valid("two operations with different names", "query A { version } query B { version }")

  // 5.2.2.1 Lone Anonymous Operation

  invalid(
    "an anonymous operation next to a named one",
    "{ version } query A { version }",
    LoneAnonymousOperation,
    "anonymous operation must be the only operation"
  )

  valid("a lone anonymous operation", "{ version }")

  // 5.3.1 Field Selections

  invalid(
    "a field the type does not define",
    "{ dog { meowVolume } }",
    FieldSelections,
    "type 'Dog' has no field 'meowVolume'"
  )

  invalid(
    "an alias cannot rename an undefined field to a defined one",
    "{ dog { barkVolume: kawVolume } }",
    FieldSelections,
    "has no field 'kawVolume'"
  )

  valid("an alias may rename a defined field", "{ dog { name: nickname } }")
  valid("an interface field on an interface", "{ pet { name } }")
  valid("__typename on a union", "{ catOrDog { __typename } }")

  invalid(
    "a field selection on a union selects a member field directly",
    "{ catOrDog { name } }",
    FieldSelections,
    "type 'CatOrDog' has no field 'name'"
  )

  invalid(
    "introspection is explicitly unsupported",
    "{ __schema { types { name } } }",
    Unsupported,
    "not implemented yet"
  )

  // 5.3.2 Field Selection Merging (partial: arguments and response keys, not cross-fragment)

  invalid(
    "the same response key with different arguments",
    """{ search(term: "a") { name } search(term: "b") { name } }""",
    FieldSelectionMerging,
    "selected twice with different arguments"
  )

  invalid(
    "one alias for two different fields",
    "{ dog { x: name x: nickname } }",
    FieldSelectionMerging,
    "response key 'x' is used for both 'name' and 'nickname'"
  )

  valid("the same field twice with the same arguments", """{ search(term: "a") { name } search(term: "a") { name } }""")

  // 5.3.3 Leaf Field Selections

  invalid(
    "a selection set on a scalar",
    "{ dog { name { length } } }",
    LeafFieldSelections,
    "must not have a selection set"
  )

  invalid("an object field without a selection set", "{ dog }", LeafFieldSelections, "must have a selection set")

  valid("an object field with a selection set", "{ dog { name } }")

  // 5.4.1 Argument Names

  invalid(
    "an argument the field does not define",
    """{ search(term: "a", nope: 1) { name } }""",
    ArgumentNames,
    "has no argument 'nope'"
  )

  invalid(
    "an argument the directive does not define",
    "{ dog { name @include(if: true, nope: 1) } }",
    ArgumentNames,
    "has no argument 'nope'"
  )

  valid("a defined argument", """{ search(term: "a") { name } }""")

  // 5.4.2 Argument Uniqueness

  invalid(
    "the same argument twice",
    """{ search(term: "a", term: "b") { name } }""",
    ArgumentUniqueness,
    "has argument 'term' 2 times"
  )

  // 5.4.2.1 Required Arguments

  invalid("a required argument omitted", "{ search { name } }", RequiredArguments, "requires argument 'term'")
  invalid(
    "a required argument omitted on a nested field",
    "{ dog { doesKnowCommand } }",
    RequiredArguments,
    "requires argument 'dogCommand'"
  )
  invalid(
    "a required directive argument omitted",
    "{ dog { name @skip } }",
    RequiredArguments,
    "requires argument 'if'"
  )

  valid("a non-null argument with a default may be omitted", "{ withDefault }")

  // 5.5.1.1 Fragment Name Uniqueness

  invalid(
    "two fragments with the same name",
    "fragment F on Dog { name } fragment F on Dog { nickname } { dog { ...F } }",
    FragmentNameUniqueness,
    "fragment 'F' is defined 2 times"
  )

  // 5.5.1.2 Fragment Spread Type Existence

  invalid(
    "a fragment on a type that does not exist",
    "fragment F on Nope { name } { dog { ...F } }",
    FragmentTypeExistence,
    "type condition 'Nope', which is not defined"
  )

  invalid(
    "a fragment on a scalar",
    "fragment F on String { length } { dog { ...F } }",
    FragmentTypeExistence,
    "which is a scalar, not a composite type"
  )

  invalid(
    "a spread of an undefined fragment",
    "{ dog { ...Nope } }",
    FragmentTypeExistence,
    "fragment 'Nope' is not defined"
  )

  invalid(
    "an inline fragment on a scalar",
    "{ dog { ... on String { length } } }",
    FragmentTypeExistence,
    "which is a scalar, not a composite type"
  )

  valid("a fragment spread of a defined fragment", "fragment F on Dog { name } { dog { ...F } }")
  valid("an inline fragment on a matching object", "{ pet { ... on Dog { barkVolume } } }")
  valid("an inline fragment without a type condition", "{ dog { ... { name } } }")

  // 5.5.2.1 Fragment Spreads Must Not Form Cycles

  invalid(
    "two fragments spreading each other",
    "fragment A on Dog { ...B } fragment B on Dog { ...A } { dog { ...A } }",
    FragmentCycles,
    "form a cycle"
  )

  invalid("a fragment spreading itself", "fragment A on Dog { ...A } { dog { ...A } }", FragmentCycles, "form a cycle")

  valid(
    "a diamond-shaped fragment graph",
    "fragment A on Dog { ...B ...C } fragment B on Dog { name } fragment C on Dog { ...B } { dog { ...A } }"
  )

  // 5.7.1 / 5.7.2 / 5.7.3 Directives

  invalid("an undefined directive", "{ dog { name @nope } }", DirectiveNames, "directive '@nope' is not defined")

  invalid("a directive on an operation", "query @skip(if: true) { dog { name } }", DirectiveLocations, "not allowed")

  invalid(
    "the same directive twice on one location",
    "{ dog { name @skip(if: true) @skip(if: false) } }",
    UniqueDirectivesPerLocation,
    "applied 2 times"
  )

  invalid(
    "an unimplemented directive fails loudly",
    "{ dog { name @defer } }",
    Unsupported,
    "'@defer' is not implemented yet"
  )

  valid("a directive in a valid location", "{ dog { name @include(if: true) } }")

  // 5.8.1 Variable Uniqueness

  invalid(
    "the same variable defined twice",
    "query Q($a: Int, $a: String) { version }",
    VariableUniqueness,
    "variable '$a' is defined 2 times"
  )

  // 5.8.2 Variables Are Input Types

  invalid(
    "a variable of an object type",
    "query Q($d: Dog) { dog { name } }",
    VariablesAreInputTypes,
    "which is a object, not an input type"
  )

  invalid(
    "a variable of an undefined type",
    "query Q($d: Nope) { dog { name } }",
    VariablesAreInputTypes,
    "which is not defined"
  )

  valid("a variable of an enum type", "query Q($cmd: DogCommand!) { dog { doesKnowCommand(dogCommand: $cmd) } }")
  valid("a variable of an input object type", "mutation Q($p: PetInput!) { addPet(input: $p) { name } }")

  // 5.8.3 All Variable Uses Defined

  invalid(
    "an undefined variable",
    "query Q { dog { doesKnowCommand(dogCommand: $cmd) } }",
    UndefinedVariables,
    "variable '$cmd' is used but not defined by operation 'Q'"
  )

  invalid(
    "an undefined variable used only inside a spread fragment",
    "query Q { dog { ...F } } fragment F on Dog { doesKnowCommand(dogCommand: $cmd) }",
    UndefinedVariables,
    "variable '$cmd' is used but not defined"
  )

  valid(
    "a variable defined by the operation and used inside a spread fragment",
    "query Q($cmd: DogCommand!) { dog { ...F } } fragment F on Dog { doesKnowCommand(dogCommand: $cmd) }"
  )

  // operation type and operation selection

  valid("a mutation when the schema has one", "mutation { addPet(input: { name: \"Rex\" }) { name } }")

  invalid(
    "a mutation when the schema has no mutation root",
    "mutation { a }",
    OperationType,
    "no mutation root type",
    schema = plain
  )

  valid("a query on the plain schema", "{ a }", schema = plain)

  test("an operationName selects among several operations") {
    val document = "query A { version } mutation B { addPet(input: { name: \"Rex\" }) { name } }"
    assertEquals(Executable.validate(spec, document, Some("A")).map(_.operation.kind), Right(OperationKind.Query))
    assertEquals(Executable.validate(spec, document, Some("B")).map(_.operation.kind), Right(OperationKind.Mutation))
  }

  test("an operationName is required when the document has several operations") {
    val result = Executable.validate(spec, "query A { version } query B { version }", None)
    assert(result.left.exists(_.toChain.exists(_.rule == OperationSelection)))
  }

  test("an unknown operationName is an error") {
    val result = Executable.validate(spec, "query A { version }", Some("Nope"))
    assert(result.left.exists(_.toChain.exists(e => e.rule == OperationSelection && e.render.contains("'Nope'"))))
  }

  test("subscriptions are rejected as unimplemented") {
    val result = Executable.validate(spec, "subscription { version }")
    assert(result.left.exists(_.toChain.exists(e => e.rule == Unsupported && e.render.contains("subscriptions"))))
  }

  // the pet shop query from the pitch, as the planner will receive it

  test("the pet shop query validates and keeps aliases intact") {
    val query =
      """query AdoptionReport($first: Int) {
        |  dogs: pets(species: DOG, first: $first) {
        |    name
        |    owner { name email }
        |    ...petFields
        |  }
        |}
        |fragment petFields on Pet { id species }
        |""".stripMargin

    Executable.validate(shop, query, Some("AdoptionReport")) match {
      case Left(errors)     => fail(errorList(errors))
      case Right(validated) =>
        val operation = validated.operation
        assertEquals(operation.kind, OperationKind.Query)
        assertEquals(operation.name, Some("AdoptionReport"))
        operation.selections match {
          case List(field: FieldSelection) =>
            assertEquals(field.responseKey, "dogs")
            assertEquals(field.name, "pets")
            assertEquals(
              field.selections.map {
                case f: FieldSelection => f.responseKey
                case s: FragmentSpread => "..." + s.name
                case _: InlineFragment => "..."
              },
              List("name", "owner", "...petFields")
            )
          case other                       => fail(s"expected one root field, got $other")
        }
    }
  }
}
