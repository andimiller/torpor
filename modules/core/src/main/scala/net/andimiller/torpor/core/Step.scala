package net.andimiller.torpor.core

/** One visit to a suspending resolver: finish now, or park the whole run with a handle that a webhook can complete. */
enum Step[+A, +H] {
  case Complete(value: A)
  case Suspend(handle: H)
}

object Step {
  def complete[A](value: A): Step[A, Nothing] = Step.Complete(value)
  def suspend[H](handle: H): Step[Nothing, H] = Step.Suspend(handle)
}
