package pli

class OpcodeSpec extends Spec {
  // exit

  "The exit instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.exit) should be (1)
  }

  it should "not take parameters" in {
    Opcode.hasParameter(Opcode.exit) should be (false)
  }

  it should "print as 'exit'" in {
    Opcode.toString(Opcode.exit, 42) should be ("exit")
  }

  // iadd

  "The iadd instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.iadd) should be (1)
  }

  it should "not take parameters" in {
    Opcode.hasParameter(Opcode.iadd) should be (false)
  }

  it should "print as 'iadd'" in {
    Opcode.toString(Opcode.iadd, 42) should be ("iadd")
  }

  // isub

  "The isub instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.isub) should be (1)
  }

  it should "not take parameters" in {
    Opcode.hasParameter(Opcode.isub) should be (false)
  }

  it should "print as 'isub'" in {
    Opcode.toString(Opcode.isub, 42) should be ("isub")
  }

  // imul

  "The imul instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.imul) should be (1)
  }

  it should "not take parameters" in {
    Opcode.hasParameter(Opcode.imul) should be (false)
  }

  it should "print as 'imul'" in {
    Opcode.toString(Opcode.imul, 42) should be ("imul")
  }

  // iconst

  "The iconst instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.iconst) should be (1)
  }

  it should "take a parameter" in {
    Opcode.hasParameter(Opcode.iconst) should be (true)
  }

  it should "print as 'iconst param'" in {
    Opcode.toString(Opcode.iconst, 42) should be ("iconst 42")
  }

  // goto

  "The goto instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.goto) should be (1)
  }

  it should "take a parameter" in {
    Opcode.hasParameter(Opcode.goto) should be (true)
  }

  it should "print as 'goto param'" in {
    Opcode.toString(Opcode.goto, 42) should be ("goto 42")
  }

  // ifeq

  "The ifeq instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.ifeq) should be (1)
  }

  it should "take a parameter" in {
    Opcode.hasParameter(Opcode.ifeq) should be (true)
  }

  it should "print as 'ifeq param'" in {
    Opcode.toString(Opcode.ifeq, 42) should be ("ifeq 42")
  }

  // ifne

  "The ifne instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.ifne) should be (1)
  }

  it should "take a parameter" in {
    Opcode.hasParameter(Opcode.ifne) should be (true)
  }

  it should "print as 'ifne param'" in {
    Opcode.toString(Opcode.ifne, 42) should be ("ifne 42")
  }

  // print

  "The print instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.print) should be (1)
  }

  it should "not take parameters" in {
    Opcode.hasParameter(Opcode.print) should be (false)
  }

  it should "print as 'print'" in {
    Opcode.toString(Opcode.print, 42) should be ("print")
  }

  // iload

  "The iload instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.iload) should be (1)
  }

  it should "take a parameter" in {
    Opcode.hasParameter(Opcode.iload) should be (true)
  }

  it should "print as 'iload param'" in {
    Opcode.toString(Opcode.iload, 42) should be ("iload 42")
  }

  // istore

  "The istore instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.istore) should be (1)
  }

  it should "take a parameter" in {
    Opcode.hasParameter(Opcode.istore) should be (true)
  }

  it should "print as 'istore param'" in {
    Opcode.toString(Opcode.istore, 42) should be ("istore 42")
  }

  // pop

  "The pop instruction" should
  "have an unique opcode"  in {
    Opcode.values.count(_ == Opcode.pop) should be (1)
  }

  it should "not take parameters" in {
    Opcode.hasParameter(Opcode.pop) should be (false)
  }

  it should "print as 'pop'" in {
    Opcode.toString(Opcode.pop, 42) should be ("pop")
  }
}
