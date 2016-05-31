package pli

/** Opcodes of individual bytecode instructions. */
object Opcode {
  /** The opcode for the `exit` instruction.
    *
    * The `exit` instruction terminates execution of the program.
    */
  val exit = 0

  /** The opcode for the `iadd` instruction.
    *
    * The `iadd` instruction pops two numbers from the stack and
    * then pushes the sum of the two numbers on the stack.
    */
  val iadd = 1

  /** The opcode for the `isub` instruction.
    *
    * The `isub` instruction pops two numbers from the stack and
    * then pushes the difference of the two numbers on the stack.
    */
  val isub = 2

  /** The opcode for the `imul` instruction.
    *
    * The `imul` instruction pops two numbers from the stack and
    * then pushes the product of the two numbers on the stack.
    */
  val imul = 3

  /** The opcode for the `iconst` instruction.
    *
    * The `iconst` instruction reads the number following the
    * `iconst` instruction from the bytecode and then pushes this
    * number on the stack. Execution continues with the opcode
    * after the number after the `iconst` instruction.
    */
  val iconst = 5

  /** The opcode for the `goto` instruction.
    *
    * The `goto` instruction reads the number following the
    * `goto` instruction from the bytecode and then arranges for
    * execution to continue at a different adress in the
    * bytecode. The target adress of this jump is computed by
    * adding the number from the bytecode to the adress of the
    * `goto` instruction.
    */
  val goto = 6

  /** The opcode for the `ifeq` instruction.
    *
    * The `ifeq` instruction pops one number from the stack and
    * reads the number following the `ifeq` instruction from the
    * bytecode. If the number from the stack is equal to 0, the
    * `ifeq` instruction then arranges for execution to continue
    * at a different adress in the bytecode. The target adress of
    * this jump is computed by adding the number from the
    * bytecode to the adress of the `ifeq` instruction. But if
    * the number from the stack is different from 0, execution
    * continues after the number after the `ifeq` instruction.
    */
  val ifeq = 7

  /** The opcode for the `ifne` instruction.
    *
    * The `ifne` instruction pops one number from the stack and
    * reads the number following the `ifne` instruction from the
    * bytecode. If the number from the stack is different from 0,
    * the `ifne` instruction then arranges for execution to
    * continue at a different adress in the bytecode. The target
    * adress of this jump is computed by adding the number from
    * the bytecode to the adress of the `ifne` instruction. But
    * if the number from the stack is equal to 0, execution
    * continues after the number after the `ifne` instruction.
    */
  val ifne = 8

  /** The opcode for the `print` instruction.
    *
    * The `print` instruction pops a number from the stack and
    * then prints this number.
    */
  val print = 9

  /** The opcode for the `iload` instruction.
    *
    * The `iload` instruction reads the number following the
    * `iload` instruction from the bytecode. If the number from
    * the bytecode is n, the `iload` instruction then fetches the
    * nth item from the stack without removing it from the stack,
    * and then pushes this number on the stack.
    *
    * For example, if the top of the stack is 10 and the second
    * number on the stack is 20, `iload 0` pushes 10 on the
    * stack, and `iload 1` pushes 20 on the stack.
    */
  val iload = 10

  /** The opcode for the `istore` instruction.
    *
    * The `istore` instruction reads the number following the
    * `istore` instruction from the bytecode. If the number from
    * the bytecode is n, the `istore` instruction pops a number
    * from the stack and stores the number from the stack as the
    * nth element of the stack.
    *
    * For example, if the stack is `10, 20, 30`, then after
    * `istore 0`, the stack would be `10, 30`. And after `istore
    * 1`, the stack would be `20, 10`.
    */
  val istore = 11

  /** The opcode for the `pop` instruction.
    *
    * The `pop` instruction pops a number from the stack and
    * ignores it.
    */
  val pop = 12

  /** All opcodes. */
  val values =
    Seq(exit, iadd, isub, imul, iconst, goto, ifeq, ifne, print,
      iload, istore, pop)

  /** prints the opcode to a human-readable representation. */
  def toString(opcode: Int): String =
    opcode match {
      case Opcode.exit => "exit"
      case Opcode.iadd => "iadd"
      case Opcode.isub => "isub"
      case Opcode.imul => "imul"
      case Opcode.iconst => "iconst"
      case Opcode.goto => "goto"
      case Opcode.ifeq => "ifeq"
      case Opcode.ifne => "ifne"
      case Opcode.print => "print"
      case Opcode.iload => "iload"
      case Opcode.istore => "istore"
      case Opcode.pop => "pop"
    }

  /** Returns whether the opcode reads a parameter from the
    * bytecode. */
  def hasParameter(opcode: Int): Boolean =
    opcode match {
      case Opcode.iconst | Opcode.goto | Opcode.ifeq |
          Opcode.ifne | Opcode.iload | Opcode.istore => true
      case _ => false
    }

  /** Prints the opcode and the parameter to a human-readable
    * representation. For parameterless opcodes, the parameter is
    * ignored. */
  def toString(opcode: Int, parameter: Int): String =
    if (Opcode.hasParameter(opcode)) {
      Opcode.toString(opcode) + " " + parameter
    } else {
      Opcode.toString(opcode)
    }

  /** Prints the whole bytecode array to a human-readable
    * representation. */
  def toString(bytecode: Array[Int]): String = {
    val builder = new StringBuilder
    var index = 0
    while (index < bytecode.size) {
      val opcode = bytecode(index)
      if (Opcode.hasParameter(opcode)) {
        val parameter = bytecode(index + 1)
        builder ++= Opcode.toString(opcode, parameter)
        builder ++= "\n"
        index += 2
      } else {
        builder ++= Opcode.toString(opcode)
        builder ++= "\n"
        index += 1
      }
    }
    builder.result
  }
}
