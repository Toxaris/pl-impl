package pli

import collection.mutable

/** Factory methods for creating symbol tables. */
object Table {
  def apply[A, B]() =
    new Table[A, B]
}

/** Mutable symbol table with support for nested scopes.
  */
class Table[A, B] {
  /** Bindings (and what they shadow). */
  val bindings = mutable.HashMap[A, mutable.Stack[B]]()

  /** Which keys are bound in all the scopes. */
  val scopes = mutable.Stack(mutable.HashMap[A, mutable.Stack[B]]())

  /** Retrieves the value according to the currently visible binding. */
  def apply(key: A): B =
    bindings.getOrElseUpdate(key, mutable.Stack[B]())(0)

  /** Updates the currently visible binding. */
  def update(key: A, value: B) {
    bindings(key)(0) = value
  }

  /** Adds a binding to the innermost scope. */
  def bind(key: A, value: B) {
    val stack = bindings.getOrElseUpdate(key, mutable.Stack[B]())
    stack.push(value)
    scopes(0)(key) = stack
  }

  /** Checks whether a key is bound in the innermost scope. */
  def bound(key: A) =
    scopes(0).contains(key)

  /** Checks whether a key is bound at all. */
  def contains(key: A) =
    bindings.contains(key) && !(bindings(key).isEmpty)

  /** Enters a nested scope. */
  def enter() {
    scopes.push(mutable.HashMap[A, mutable.Stack[B]]())
  }

  /** Leaves a nested scope. */
  def leave() {
    for (stack <- scopes.pop().values) {
      stack.pop()
    }
  }
}

