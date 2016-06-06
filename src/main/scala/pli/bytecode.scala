package pli

/** Factory methods for [[Bytecode]] instances. */
object Bytecode {
  /** Creates an empty bytecode builder. */
  def apply() =
    new Bytecode
}

/** Builder for bytecode arrays. */
class Bytecode extends Buffer {
  /** The adress of the next element that is added to the bytecode
    * array. */
  def adress = length


  /** Patches a jump distance belonging to a jump instruction at
    * the given adress. */
  def patch(adress: Int, target: Int) {
    put(adress + 1, target - adress)
  }

  /** Appends an integer to the bytecode array and returns the
    * adress of the newly added integer. */
  def append(value: Int): Int = {
    val result = length
    put(adress, value)
    result
  }

  /** Appends an `iadd` instruction and returns the adress of the
    * newly added instructon. */
  def iadd() =
    append(Opcode.iadd)

  /** Appends an `isub` instruction and returns the adress of the
    * newly added instructon. */
  def isub() =
    append(Opcode.isub)

  /** Appends an `imul` instruction and returns the adress of the
    * newly added instructon. */
  def imul() =
    append(Opcode.imul)

  /** Appends an `iconst` instruction and returns the adress of the
    * newly added instructon. */
  def iconst(value: Int) = {
    val result = append(Opcode.iconst)
    append(value)
    result
  }

  /** Appends a `goto` instruction and returns the adress of the
    * newly added instruction.
    *
    * @param target the absolute adress of the target of this `goto`
    * instruction.
    */
  def goto(target: Int = 0) = {
    val base = append(Opcode.goto)
    append(target - base)
    base
  }

  /** Appends an `ifeq` instruction and returns the adress of the
    * newly added instruction.
    *
    * @param target the absolute adress of the target of this `ifeq`
    * instruction.
    */
  def ifeq(target: Int = 0) = {
    val base = append(Opcode.ifeq)
    append(target - base)
    base
  }

  /** Appends an `ifne` instruction and returns the adress of the
    * newly added instruction.
    *
    * @param target the absolute adress of the target of this `ifne`
    * instruction.
    */
  def ifne(target: Int = 0) = {
    val base = append(Opcode.ifne)
    append(target - base)
    base
  }

  /** Appends a `print` instruction and returns the adress of the
    * newly added instruction.
    */
  def print() =
    append(Opcode.print)

  /** Appends an `iload` instruction and returns the adress of the
    * newly added instruction.
    */
  def iload(offset: Int) = {
    val result = append(Opcode.iload)
    append(offset)
    result
  }

  /** Appends a `istore` instruction and returns the adress of the
    * newly added instruction.
    */
  def istore(offset: Int) = {
    val result = append(Opcode.istore)
    append(offset)
    result
  }

  /** Appends a `pop` instruction and returns the adress of the new
    * added instruction. */
  def pop() =
    append(Opcode.pop)

  /** Appends an `exit` instruction and returns the adress of the
    * newly added instruction.
    */
  def exit() =
    append(Opcode.exit)

  override def toString() =
    Opcode.toString(result)
}
