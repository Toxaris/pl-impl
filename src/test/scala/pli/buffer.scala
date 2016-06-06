package pli

class BufferSpec extends Spec {
  // result

  "Buffer.result" when "called on a fresh buffer" should
  "return an empty array" in {
    val arr = Buffer()
    arr.result should have size 0
  }

  // resize

  "Buffer.resize" should "resize the buffer to contain the given adress" in {
    val arr = Buffer()
    for (adress <- Seq(20, 30, 100, 24039)) {
      arr.resize(adress)
      arr.buffer.length should be > adress
    }
  }

  it should "keep the old array contents" in  {
    val arr = Buffer()
    arr.put(0, 27)
    arr.put(1, 42)
    arr.put(2, 117)
    val oldContents = arr.result
    arr.resize(100)
    val newContents = arr.result
    newContents.take(oldContents.length) should be (oldContents)
  }

  it should "not allocate anything if the buffer is already big enough" in {
    val arr = Buffer()
    arr.resize(100)
    val oldBuffer = arr.buffer
    arr.resize(42)
    val newBuffer = arr.buffer
    newBuffer should be (oldBuffer)
  }

  it should "update the length if necessary" in {
    val arr = Buffer()
    arr.resize(200)
    arr.length should be (201)
    arr.resize(100)
    arr.length should be (201)
    arr.resize(1003)
    arr.length should be (1004)
  }
}
