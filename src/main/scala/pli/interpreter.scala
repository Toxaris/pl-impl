package pli

/** Interpreter.
  */
class Interpreter {
  /** Variable bindings. */
  val bindings = Table[String, Value]()

  /** Returns whether a value should be treated as true.
    */
  def isTrue(value: Value) =
    value match {
      case NumericValue(0) => false
      case NumericValue(_) => true
    }

  /** Executes a program.
    *
    * Executes the statements in the program as a block.
    */
  def run(program: Program) {
    program match {
      case Program(_, _, statements) =>
        run(statements)
    }
  }

  /** Executes a block.
    *
    * Executes the statements in the block in sequence, in a
    * nested scope so that:
    *
    *   - variable bindings inside the block hide variable
    *     bindings for the same variable name outside the block
    *   - variable bindings inside the block are invisible
    *     outside the block.
    */
  def run(statements: Seq[Statement]) {
    bindings.enter()
    for (statement <- statements) {
      run(statement)
    }
    bindings.leave()
  }

  /** Executes a statement. */
  def run(statement: Statement) {
    statement match {
      case Var(name, value) =>
        bindings.bind(name, eval(value))
      case Assignment(name, value) =>
        bindings(name) = eval(value)
      case Print(value) =>
        print(eval(value))
      case While(condition, body) =>
        while (isTrue(eval(condition))) {
          run(body)
        }
      case If(condition, thenBranch, elseBranch) =>
        if (isTrue(eval(condition))) {
          run(thenBranch)
        } else {
          run(elseBranch)
        }
      case Block(body) =>
        run(body)
    }
  }

  /** Evaluate an expression. */
  def eval(expression: Expression): Value =
    expression match {
      case Variable(name) =>
        bindings(name)
      case Literal(value) =>
        NumericValue(value)
      case Addition(lhs, rhs) =>
        eval(lhs) match {
          case NumericValue(a) =>
            eval(rhs) match {
              case NumericValue(b) =>
                NumericValue(a + b)
            }
        }
      case Subtraction(lhs, rhs) =>
        eval(lhs) match {
          case NumericValue(a) =>
            eval(rhs) match {
              case NumericValue(b) =>
                NumericValue(a - b)
            }
        }
      case Multiplication(lhs, rhs) =>
        eval(lhs) match {
          case NumericValue(a) =>
            eval(rhs) match {
              case NumericValue(b) =>
                NumericValue(a * b)
            }
        }
    }
}
