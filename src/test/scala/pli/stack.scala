package pli

class StackSpec extends Spec {
  // push

  "Stack.push" should "add values to the stack" in {
    val stack = Stack(3, 4)
    stack.length should be (2)
    stack.push(42)
    stack.length should be (3)
    stack.push(27)
    stack.length should be (4)
  }

  // pop

  "Stack.pop" should "fail for empty stacks" in {
    val stack = Stack()
    intercept[java.lang.ArrayIndexOutOfBoundsException] {
      stack.pop()
    }
  }

  it should "remove values from the stack" in {
    val stack = Stack(27, 42)
    stack.length should be (2)
    stack.pop()
    stack.length should be (1)
    stack.pop()
    stack.length should be (0)
  }

  it should "return the most-recently pushed value" in {
    val stack = Stack(27, 42)
    stack.push(5)
    stack.pop should be (5)
    stack.pop should be (42)
    stack.pop should be (27)
  }

  // apply

  "Stack.apply" should "not change the stack" in {
    val stack = Stack(27, 42)
    stack(0)
    stack(1)
    stack.length should be (2)
    stack.buffer(0) should be (27)
    stack.buffer(1) should be (42)
  }

  it should "return values from the stack, counting from the end" in {
    val stack = Stack(27, 42)
    stack(0) should be (42)
    stack(1) should be (27)
  }

  // update

  "Stack.update" should "change existing values in the stack" in {
    val stack = Stack(27, 42)
    stack(0) = 10
    stack(1) = 20
    stack(0) should be (10)
    stack(1) should be (20)
  }
}
