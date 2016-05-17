package pli

class TableSpec extends Spec {
  "Table.bound" should "return false for empty tables" in {
    val table = Table[String, Int]()
    table.bound("hello") should be (false)
  }

  it should "return false for unbound variables" in {
    val table = Table[String, Int]()
    table.bind("hello", 10)
    table.bound("answer") should be (false)
  }

  it should "return false for variables bound only in an enclosing scope" in {
    val table = Table[String, Int]()
    table.bind("hello", 10)
    table.enter()
    table.bound("hello") should be (false)
    table.leave()
  }

  it should "return true for fresh variables bound in the current scope" in {
    val table = Table[String, Int]()
    table.bind("hello", 10)
    table.bound("hello") should be (true)
  }

  it should "return true for shadowing variables bound in the current scope" in {
    val table = Table[String, Int]()
    table.bind("hello", 10)
    table.enter()
    table.bind("hello", 5)
    table.bound("hello") should be (true)
    table.leave()
  }

  "Table.contains" should "return false for empty tables" in {
    val table = Table[String, Int]()
    table.contains("hello") should be (false)
  }

  it should "return false for unbound variables" in {
    val table= Table[String, Int]()
    table.bind("hello", 10)
    table.contains("answer") should be (false)
  }

  it should "return true for variables bound only in an enclosing scope" in {
    val table = Table[String, Int]()
    table.bind("hello", 10)
    table.enter()
    table.contains("hello") should be (true)
    table.leave()
  }

  it should "return true for fresh variables bound in the current scope" in {
    val table = Table[String, Int]()
    table.bind("hello", 10)
    table.contains("hello") should be (true)
  }

  it should "return true for shadowing variables bound in the current scope" in {
    val table = Table[String, Int]()
    table.bind("hello", 10)
    table.enter()
    table.bind("hello", 5)
    table.contains("hello") should be (true)
    table.leave()
  }
}
