package pli

class LexerSpec extends Spec {
  "A lexer" should "find EOF for the empty string" in {
    val lexer = Lexer.forString("")
    lexer.nextTokenType should be (EndOfFile)
  }

  it should "recognize the 'object' keyword" in {
    val lexer = Lexer.forString("object")
    lexer.nextTokenType should be (ObjectKeyword)
    lexer.readNextToken()
    lexer.nextTokenType should be (EndOfFile)
  }

  it should "reject the reserved word 'for'" in {
    val lexer = Lexer.forString ("var for = 5")
    intercept[Error] {
      lexer.readNextToken()
    }
  }

  it should "skip whitespace at the beginning of the input string" in {
    val lexer = Lexer.forString("   foo")
    lexer.startColumn should be (4)
  }
}
