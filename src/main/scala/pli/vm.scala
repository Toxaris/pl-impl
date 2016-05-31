package pli

import collection.mutable
import mutable.Stack

/** Factory methods for creating virtual machines. */
object VM {
  def apply(bytecode: Array[Int]): VM  =
    new VM(bytecode)

  def apply(bytecode: Bytecode): VM =
    new VM(bytecode.result)

  def apply(codegen: CodeGenerator): VM =
    VM(codegen.bytecode)

  def apply(ast: ASTNode): VM = {
    val codegen = new CodeGenerator()
    codegen.generate(ast)
    codegen.bytecode.exit()
    VM(codegen)
  }
}

/** Simple virtual machine, that is, bytecode interpreter. */
class VM(bytecode: Array[Int]) {
  /** adress in the bytecode array to read next */
  var codepointer = 0

  /** stack */
  var stack = Stack[Int]()

  /** whether we are currently running. */
  var running = false

  /** fetches next integer from the bytecode array */
  def fetch(): Int = {
    val result = bytecode(codepointer)
    codepointer += 1
    result
  }

  def push(value: Int) {
    stack.push(value)
  }

  def pop(): Int =
    stack.pop()

  def get(offset: Int): Int =
    stack(offset)

  def put(offset: Int, value: Int) {
    stack(offset) = value
  }

  def step() {
    val base = codepointer
    val opcode = fetch()
    if (Opcode.hasParameter(opcode)) {
      val parameter = fetch()
      execute(base, opcode, parameter)
    } else {
      execute(base, opcode)
    }
  }

  def execute(adress: Int, opcode: Int, parameter: Int = 0) {
    opcode match {
      case Opcode.iadd =>
        val rhs = pop()
        val lhs = pop()
        push(lhs + rhs)
      case Opcode.isub =>
        val rhs = pop()
        val lhs = pop()
        push(lhs - rhs)
      case Opcode.imul =>
        val rhs = pop()
        val lhs = pop()
        push(lhs * rhs)
      case Opcode.iconst =>
        push(parameter)
      case Opcode.goto =>
        codepointer = adress + parameter
      case Opcode.ifeq =>
        val condition = pop()
        if (condition == 0) {
          codepointer = adress + parameter
        }
      case Opcode.ifne =>
        val condition = pop
        if (condition != 0) {
          codepointer = adress + parameter
        }
      case Opcode.print =>
        val value = pop()
        println(value)
      case Opcode.iload =>
        push(get(parameter))
      case Opcode.istore =>
        val value = pop()
        put(parameter, value)
      case Opcode.exit =>
        running = false;
      case Opcode.pop =>
        pop()
    }
  }

  def run() {
    running = true;
    while (running) {
      step()
    }
  }
}

class DebugVM(bytecode: Array[Int]) extends VM(bytecode) {
  override def execute(adress: Int, opcode: Int, parameter: Int) {
    super.execute(adress, opcode, parameter)

    val op = Opcode.toString(opcode, parameter)

    stack match {
      case Stack() =>
        println(f"$adress%02d: $op%-12s | empty stack")
      case Stack(s1) =>
        println(f"$adress%02d: $op%-12s | $s1%4d")
      case Stack(s1, s2) =>
        println(f"$adress%02d: $op%-12s | $s2%4d $s1%4d")
      case Stack(s1, s2, s3) =>
        println(f"$adress%02d: $op%-12s | $s3%4d $s2%4d $s1%4d")
      case Stack(s1, s2, s3, s4) =>
        println(f"$adress%02d: $op%-12s | $s4%4d $s3%4d $s2%4d $s1%4d")
      case Stack(s1, s2, s3, _@_*) =>
        println(f"$adress%02d: $op%-12s |  ... $s3%4d $s2%4d $s1%4d")
    }
  }
}

/** Factory methods for creating debug virtual machines. */
object DebugVM {
  def apply(bytecode: Array[Int]): VM  =
    new DebugVM(bytecode)

  def apply(bytecode: Bytecode): VM =
    new DebugVM(bytecode.result)

  def apply(codegen: CodeGenerator): VM =
    DebugVM(codegen.bytecode)

  def apply(ast: ASTNode): VM = {
    val codegen = new CodeGenerator()
    codegen.generate(ast)
    codegen.bytecode.exit()
    DebugVM(codegen)
  }
}
