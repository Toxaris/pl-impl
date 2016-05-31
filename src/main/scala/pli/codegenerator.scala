package pli

/** Code generator. */
class CodeGenerator {
  /** Builder for bytecode array that this code generator
    * produces. */
  val bytecode = Bytecode()

  /** Size of the runtime stack after executing the bytecode
    * produced so far. */
  var stacksize = 0

  /** Association of variable names to offsets into the stack at
    * runtime, counting from the bottom of the stack. */
  val table = Table[String, Int]

  /** Computes the offset into the stack at runtime for a variable
    * binding, counting from the top of the stack. */
  def offset(name: String) =
    stacksize - table(name)

  /** Generates bytecode for the statements in a block. */
  def generate(statements: Seq[Statement]) {
    table.enter()
    for (statement <- statements) {
      generate(statement)
    }
    for (_ <- table.scopes(0)) {
      bytecode.pop()
    }
    table.leave()
  }

  // TODO let generate return the adress of the first opcode generated

  /** Generates bytecode for an AST node.
    *
    * The bytecode for an expression will have the overall effect
    * of pushing the value of the expression on the stack.
    *
    * The bytecode for a statement will have the overall effect
    * of executing the statement. The bytecode for variable
    * declaration statements pushes the initial value of the
    * newly declared variable on the stack, the bytecode for
    * other statements does not affect the stack.
    */
  def generate(node: ASTNode) {
    node match {
      case Program(_, _, body) =>
        generate(body)

      case Variable(name) =>
        bytecode.iload(offset(name))
        stacksize += 1

      case Literal(value) =>
        bytecode.iconst(value)
        stacksize += 1

      case Addition(lhs, rhs) =>
        generate(lhs)
        generate(rhs)
        bytecode.iadd()
        stacksize -= 1

      case Subtraction(lhs, rhs) =>
        generate(lhs)
        generate(rhs)
        bytecode.isub()
        stacksize -= 1

      case Multiplication(lhs, rhs) =>
        generate(lhs)
        generate(rhs)
        bytecode.imul()
        stacksize -= 1

      case Var(name, value) =>
        generate(value)
        table.bind(name, stacksize)

      case Assignment(name, value) =>
        generate(value)
        bytecode.istore(offset(name) - 1)
        stacksize -= 1

      case Print(value) =>
        generate(value)
        bytecode.print()
        stacksize -= 1

      case While(condition, body) =>
        val adressOfCondition =
          bytecode.adress
        generate(condition)
        val jumpToAfterLoop =
          bytecode.ifeq()
        stacksize -= 1
        generate(body)
        bytecode.goto(adressOfCondition)
        val adressAfterLoop =
          bytecode.adress

        bytecode.patch(jumpToAfterLoop, adressAfterLoop)

      case If(condition, thenBranch, elseBranch) =>
        generate(condition)
        val jumpToThenBranch =
          bytecode.ifne()
        stacksize -= 1
        generate(elseBranch)
        val jumpToAfterIf =
          bytecode.goto()
        val adressOfThenBranch =
          bytecode.adress
        generate(thenBranch)
        val adressAfterIf =
          bytecode.adress

        bytecode.patch(jumpToThenBranch, adressOfThenBranch)
        bytecode.patch(jumpToAfterIf, adressAfterIf)

      case Block(body) =>
        generate(body)
    }
  }
}

