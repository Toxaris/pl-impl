package pli

import java.io.PrintWriter

/** Factory methods for creating [[Pretty pretty printers]]. */
object Pretty {
  def toPrintWriter(out: PrintWriter) =
    new Pretty(out)
}

/** Turns an abstract syntax tree back into a string.
  *
  * To create a pretty printer, use the companion object’s
  * [[Pretty#toPrintWriter `toPrintWriter`]] method.
  *
  * The pretty printer is a helper component in the language
  * implementation. It can be used during debugging to print out
  * abstract syntax trees for manual inspection.
  */
class Pretty(out: PrintWriter) {
  /** Current indentation level */
  var level = 0

  /** Prints text. */
  def append(text: String) {
    out.print(text)
  }

  /** Prints integer. */
  def append(value: Int) {
    out.print(value)
  }

  /** Prints newline and then prints enough spaces to reach current
    * indentation level. */
  def newline() {
    out.println()
    out.print(" " * (2 * level))
  }

  /** Prints body. If guard is true, between parentheses. */
  def parens(guard: Boolean = true)(body: => Unit) {
    if (guard) append("(")
    body
    if (guard) append(")")
  }

  /** Prints sequence of statements in braces. */
  def append(body: Seq[Statement]) {
    append("{")
    level += 1
    for (statement <- body) {
      append(statement)
    }
    level -= 1
    newline()
    append("}")
  }

  /** Prints AST node, assuming priority context. */
  def append(node: ASTNode, context: Int = 0) {
    node match {
      case Program(name, superclass, body) =>
        newline()
        append("object ")
        append(name)
        append(" extends ")
        append(superclass)
        append(" ")
        append(body)

      case Var(name, value) =>
        newline()
        append("var ")
        append(name)
        append(" = ")
        append(value)
        append(";")

      case Assignment(name, value) =>
        newline()
        append(name)
        append(" = ")
        append(value, 4)
        append(";")

      case Print(value) =>
        newline()
        append("print(")
        append(value)
        append(");")

      case While(condition, body) =>
        newline()
        append("while (")
        append(condition)
        append(") ")
        append(body)

      case If(condition, thenBranch, Seq()) =>
        newline()
        append("if (")
        append(condition)
        append(") ")
        append(thenBranch)

      case If(condition, thenBranch, elseBranch) =>
        newline()
        append("if (")
        append(condition)
        append(") ")
        append(thenBranch)
        newline()
        append("else")
        append(elseBranch)

      case Block(body) =>
        newline()
        append(body)

      case Variable(name) =>
        append(name)

      case Literal(value) =>
        append(value)

      case Addition(lhs, rhs) =>
        parens(context > 7) {
          append(lhs, 7)
          append(" + ")
          append(rhs, 8)
        }

      case Subtraction(lhs, rhs) =>
        parens(context > 7) {
          append(lhs, 7)
          append(" - ")
          append(rhs, 8)
        }

      case Multiplication(lhs, rhs) =>
        parens(context > 8) {
          append(lhs, 8)
          append(" * ")
          append(rhs, 9)
        }
    }
  }
}

