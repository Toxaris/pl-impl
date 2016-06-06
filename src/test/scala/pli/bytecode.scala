package pli

class BytecodeSpec extends Spec {
  // result

  "Bytecode.result" should "return an empty array if no opcodes where appended" in {
    val bc = Bytecode()
    bc.result should have size 0
  }

  it should "return the bytecode array of all opcodes that were appended" in {
    val bc = Bytecode()
    bc.append(27)
    bc.append(42)
    bc.append(117)
    bc.result should be (Array(27, 42, 117))
  }

  it should "return a snapshot of the bytecode array" in {
    val bc = Bytecode()
    bc.append(27)
    bc.append(42)
    bc.append(117)
    val res = bc.result
    bc.append(123)
    bc.append(95)
    res should be (Array(27, 42, 117))
  }

  // adress

  "Bytecode.adress" when "called on an empty bytecode builder" should
  "return 0" in {
    val bc = Bytecode()
    bc.adress should be (0)
  }

  // resize

  "Bytecode.resize" should "resize the buffer to contain the given adress" in {
    val bc = Bytecode()
    for (adress <- Seq(20, 30, 100, 24039)) {
      bc.resize(adress)
      bc.buffer.length should be > adress
    }
  }

  it should "keep the old array contents" in  {
    val bc = Bytecode()
    bc.append(27)
    bc.append(42)
    bc.append(117)
    val oldContents = bc.result
    bc.resize(100)
    val newContents = bc.result
    newContents.take(oldContents.length) should be (oldContents)
  }

  it should "not allocate anything if the buffer is already big enough" in {
    val bc = Bytecode()
    bc.resize(100)
    val oldBuffer = bc.buffer
    bc.resize(42)
    val newBuffer = bc.buffer
    newBuffer should be (oldBuffer)
  }

  it should "update the length if necessary" in {
    val bc = Bytecode()
    bc.resize(200)
    bc.length should be (201)
    bc.resize(100)
    bc.length should be (201)
    bc.resize(1003)
    bc.length should be (1004)
  }

  // iadd

  "Bytecode.iadd" should "append an iadd instruction" in {
    val bc = Bytecode()
    bc.iadd()
    bc.result(0) should be (Opcode.iadd)
  }

  it should "use one slot in the bytecode" in {
    val bc = Bytecode()
    bc.iadd()
    bc.adress should be (1)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.iadd() should be (3)    
  }

  // isub

  "Bytecode.isub" should "append an isub instruction" in {
    val bc = Bytecode()
    bc.isub()
    bc.result(0) should be (Opcode.isub)
  }

  it should "use one slot in the bytecode" in {
    val bc = Bytecode()
    bc.isub()
    bc.adress should be (1)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.isub() should be (3)    
  }

  // imul

  "Bytecode.imul" should "append an imul instruction" in {
    val bc = Bytecode()
    bc.imul()
    bc.result(0) should be (Opcode.imul)
  }

   it should "use one slot in the bytecode" in {
    val bc = Bytecode()
    bc.imul()
    bc.adress should be (1)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.imul() should be (3)    
  }

  // iconst

  "Bytecode.iconst" should "append an iconst instruction" in {
    val bc = Bytecode()
    bc.iconst(42)
    bc.result(0) should be (Opcode.iconst)
  }

  it should "append the given constant" in {
    val bc = Bytecode()
    bc.iconst(42)
    bc.result(1) should be (42)
  }

  it should "use two slots in the bytecode" in {
    val bc = Bytecode()
    bc.iconst(42)
    bc.adress should be (2)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.iconst(42) should be (3)    
  }

  // goto

  "Bytecode.goto" should "append a goto instruction" in {
    val bc = Bytecode()
    bc.goto(0)
    bc.result(0) should be (Opcode.goto)
  }

  it should "append an offset computed from the given target" in {
    val bc = Bytecode()
    bc.iconst(42)
    bc.goto(5)
    bc.result(3) should be (3)

    bc.iconst(27)
    bc.goto(0)
    bc.result(7) should be (-6)
  }

  it should "use two slots in the bytecode" in {
    val bc = Bytecode()
    bc.goto(0)
    bc.adress should be (2)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.goto(100) should be (3)    
  }

  // ifeq

  "Bytecode.ifeq" should "append an ifeq instruction" in {
    val bc = Bytecode()
    bc.ifeq(0)
    bc.result(0) should be (Opcode.ifeq)
  }

  it should "use two slots in the bytecode" in {
    val bc = Bytecode()
    bc.ifeq(0)
    bc.adress should be (2)
  }

  it should "append an offset computed from the given target" in {
    val bc = Bytecode()
    bc.iconst(42)
    bc.ifeq(5)
    bc.result(3) should be (3)

    bc.iconst(27)
    bc.ifeq(0)
    bc.result(7) should be (-6)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.ifeq(100) should be (3)    
  }

  // ifne

  "Bytecode.ifne" should "append an ifne instruction" in {
    val bc = Bytecode()
    bc.ifne(0)
    bc.result(0) should be (Opcode.ifne)
  }

  it should "use two slots in the bytecode" in {
    val bc = Bytecode()
    bc.ifne(0)
    bc.adress should be (2)
  }

  it should "append an offset computed from the given target" in {
    val bc = Bytecode()
    bc.iconst(42)
    bc.ifne(5)
    bc.result(3) should be (3)

    bc.iconst(27)
    bc.ifne(0)
    bc.result(7) should be (-6)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.ifne(100) should be (3)    
  }

  // print

  "Bytecode.print" should "append a print instruction" in {
    val bc = Bytecode()
    bc.print()
    bc.result(0) should be (Opcode.print)
  }

  it should "use one slot in the bytecode" in {
    val bc = Bytecode()
    bc.print()
    bc.adress should be (1)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.print() should be (3)    
  }

  // iload

  "Bytecode.iload" should "append an iload instruction" in {
    val bc = Bytecode()
    bc.iload(42)
    bc.result(0) should be (Opcode.iload)
  }

  it should "append the given offset" in {
    val bc = Bytecode()
    bc.iload(42)
    bc.result(1) should be (42)
  }

  it should "use two slots in the bytecode" in {
    val bc = Bytecode()
    bc.iload(42)
    bc.adress should be (2)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.iload(100) should be (3)    
  }

  // istore


  "Bytecode.istore" should "append an istore instruction" in {
    val bc = Bytecode()
    bc.istore(42)
    bc.result(0) should be (Opcode.istore)
  }

  it should "append the given offset" in {
    val bc = Bytecode()
    bc.istore(42)
    bc.result(1) should be (42)
  }

  it should "use two slots in the bytecode" in {
    val bc = Bytecode()
    bc.istore(42)
    bc.adress should be (2)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.istore(100) should be (3)    
  }


  // exit

  "Bytecode.exit" should "append an exit instruction" in {
    val bc = Bytecode()
    bc.exit()
    bc.result(0) should be (Opcode.exit)
  }

  it should "use one slot in the bytecode" in {
    val bc = Bytecode()
    bc.exit()
    bc.adress should be (1)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.exit() should be (3)    
  }

  // pop

  "Bytecode.pop" should "append a pop instruction" in {
    val bc = Bytecode()
    bc.pop()
    bc.result(0) should be (Opcode.pop)
  }

  it should "use one slot in the bytecode" in {
    val bc = Bytecode()
    bc.pop()
    bc.adress should be (1)
  }

  it should "return the adress of the appended instruction" in {
    val bc = Bytecode()
    bc.append(1)
    bc.append(2)
    bc.append(3)
    bc.pop() should be (3)    
  }
}
