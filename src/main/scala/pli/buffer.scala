package pli

import collection.mutable;

/** Factory methods for [Buffer buffers].*/
object Buffer {
  /** Creates an empty buffer. */
  def apply() =
    new Buffer
}

/** Arrays of int values that grow automatically when necessary. */
class Buffer {
  /** The underlying int array. */
  var buffer = new Array[Int](1)

  /** The current length of the int array. */
  var length = 0

  /** Ensures that the buffer is big enough to contain the given adress. */
  def resize(adress: Int) {
    var size = buffer.size
    while (adress >= size) {
      size *= 2
    }

    if (size > buffer.size) {
      val old = buffer
      buffer = new Array[Int](size)
      old.copyToArray(buffer)
    }

    if (adress >= length) {
      length = adress + 1
    }
  }

  /** Modifies the entry in the buffer at the specified adress,
    * counting from the beginning. */
  def put(adress: Int, value: Int) {
    resize(adress)
    buffer(adress) = value
  }

  /** Returns the entry in the buffer at the specified adress,
    * counting from the beginning. */
  def get(adress: Int): Int = {
    resize(adress)
    buffer(adress)
  }

  /** Returns a snapshot of the underlying array. */
  def result =
    buffer.take(length)
}
