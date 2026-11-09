package net.andimiller.torpor.schema

/** The validation rules implemented in M0. Labels cite the spec section where there is one; `Unsupported` carries the
  * features that are deliberately not implemented yet and must fail loudly rather than half-work.
  */
enum ValidationRule(val label: String) {
  case DocumentSyntax extends ValidationRule("document syntax")
  case ExecutableDefinitions extends ValidationRule("5.1.1 Executable Definitions")
  case OperationNameUniqueness extends ValidationRule("5.2.1.1 Operation Name Uniqueness")
  case LoneAnonymousOperation extends ValidationRule("5.2.2.1 Lone Anonymous Operation")
  case OperationSelection extends ValidationRule("operation selection")
  case OperationType extends ValidationRule("operation type")
  case FragmentNameUniqueness extends ValidationRule("5.5.1.1 Fragment Name Uniqueness")
  case FragmentTypeExistence extends ValidationRule("5.5.1.2 Fragment Spread Type Existence")
  case FragmentCycles extends ValidationRule("5.5.2.1 Fragment Spreads Must Not Form Cycles")
  case FieldSelections extends ValidationRule("5.3.1 Field Selections")
  case FieldSelectionMerging extends ValidationRule("5.3.2 Field Selection Merging")
  case LeafFieldSelections extends ValidationRule("5.3.3 Leaf Field Selections")
  case ArgumentNames extends ValidationRule("5.4.1 Argument Names")
  case ArgumentUniqueness extends ValidationRule("5.4.2 Argument Uniqueness")
  case RequiredArguments extends ValidationRule("5.4.2.1 Required Arguments")
  case DirectiveNames extends ValidationRule("5.7.1 Directives Are Defined")
  case DirectiveLocations extends ValidationRule("5.7.2 Directives Are In Valid Locations")
  case UniqueDirectivesPerLocation extends ValidationRule("5.7.3 Directives Are Unique Per Location")
  case VariableUniqueness extends ValidationRule("5.8.1 Variable Uniqueness")
  case VariablesAreInputTypes extends ValidationRule("5.8.2 Variables Are Input Types")
  case UndefinedVariables extends ValidationRule("5.8.3 All Variable Uses Defined")
  case Unsupported extends ValidationRule("unsupported")
}

final case class ValidationError(rule: ValidationRule, detail: String, origin: Origin) {
  def render: String = s"${origin.render}: ${rule.label}: $detail"
}
