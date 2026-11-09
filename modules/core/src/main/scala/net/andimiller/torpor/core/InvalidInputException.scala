package net.andimiller.torpor.core

/** Raised when the values a resolver was given cannot be decoded (arguments, parent values, batch keys).
  *
  * It is an exception so it can cross the resolver boundary, but the executor catches it and reports `INVALID_INPUT`
  * rather than letting it abort the run.
  */
final class InvalidInputException(message: String) extends RuntimeException(message)
