package pli

/** Factory methods and pattern matching support for [[Stack]]. */
object Stack {
  /** Creates a stack with the given contents. */
  def apply(values: Int*) = {
    val stack = new Stack()
    for (value <- values) {
      stack.push(value)
    }
    stack
  }

  def unapplySeq(s: Stack): Some[Seq[Int]] =
    Some(s.result)
}

/** Integer stacks that automatically grow when necessary . */
class Stack extends Buffer {
  /** Pushes an integer on the stack */
  def push(value: Int) = {
    put(length, value)
  }

  /** Pops an integer from the stack and returns it. */
  def pop(): Int = {
    val result = this(0)
    length -= 1
    result
  }

  /** Overwrites the nth element of the stack, counting from the
    * most recently pushed element. */
  def update(offset: Int, value: Int) {
    put(length - offset - 1, value)
  }

  /** Fetches the nth element of the stack, counting from the most
    * recently pushed element. */
  def apply(offset: Int): Int =
    get(length - offset - 1)
}
