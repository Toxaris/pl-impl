package pli

class LexerSpec extends Spec {
  "A lexer" should "find EOF for the empty string" in {
    val lexer = Lexer.forString("")
    lexer.nextTokenType should be (EndOfFile)
  }
}
