package pli

/** Factory methods for creating [[Parser parsers]]. */
object Parser {
  /** Creates a parser for the source code in the given file. */
  def forLexer(lexer: Lexer): Parser =
    new Parser(lexer)

  /** Creates a parser for the given source code. */
  def forString(text: String): Parser =
    forLexer(Lexer.forString(text))

  /** Creates a parser for the given lexer. */
  def forFile(filename: String): Parser =
    forLexer(Lexer.forFile(filename))
}

class Parser(lexer: Lexer) {
  import lexer._

  def parseProgram: Program = {
    expect(ObjectKeyword)
    val name = parseName
    expect(ExtendsKeyword)
    val superclass = parseName
    expect(OpeningBrace)
    val body = Seq.newBuilder[Statement]
    while (!check(ClosingBrace)) {
      body += parseStatement
    }
    Program(name, superclass, body.result)
  }

  def parseStatement: Statement = {
    if (check(VarKeyword)) {
      val name = parseName
      expect(EqualsOperator)
      val value = parseExpression
      expect(Semicolon)
      Var(name, value)
    } else if (at(Identifier)) {
      val name = parseName
      expect(EqualsOperator)
      val value = parseExpression
      expect(Semicolon)
      Assignment(name, value)
    } else if (check(PrintKeyword)) {
      expect(OpeningParenthesis)
      val value = parseExpression
      expect(ClosingParenthesis)
      expect(Semicolon)
      Print(value)
    } else if (check(WhileKeyword)) {
      expect(OpeningParenthesis)
      val condition = parseExpression
      expect(ClosingParenthesis)
      val body = parseStatement
      While(condition, body)
    } else if (check(IfKeyword)) {
      expect(OpeningParenthesis)
      val condition = parseExpression
      expect(ClosingParenthesis)
      val thenBranch = parseStatement
      if (check(ElseKeyword)) {
        val elseBranch = parseStatement
        If(condition, thenBranch, elseBranch)
      } else {
        If(condition, thenBranch, Block(Seq()))
      }
    } else if (check(OpeningBrace)) {
      val body = Seq.newBuilder[Statement]
      while (!check(ClosingBrace)) {
        body += parseStatement
      }
      Block(body.result)
    } else {
      throw new Error("unexpected token")
    }
  }

  def parseExpression: Expression = {
    var result = parseSummand
    while (true) {
      if (check(PlusOperator)) {
        result = Addition(result, parseSummand)
      } else if (check(MinusOperator)) {
        result = Subtraction(result, parseSummand)
      } else {
        return result
      }
    }

    // can never happen, but Scala doesn't realize that :(
    return null
  }

  def parseSummand: Expression = {
    var result = parseFactor
    while (true) {
      if (check(TimesOperator)) {
        result = Multiplication(result, parseFactor)
      } else {
        return result
      }
    }

    // can never happen, but Scala doesn't realize that :(
    return null
  }

  def parseFactor: Expression =
    if (at(Identifier)) {
      Variable(parseName)
    } else if (at(IntegerLiteral)) {
      Literal(parseInteger)
    } else if (check(OpeningParenthesis)) {
      val result = parseExpression
      expect(ClosingParenthesis)
      result
    } else {
      throw new Error("unexpected token")
    }

  def parseName: String = {
    val text = nextTokenText.toString
    expect(Identifier)
    text
  }

  def parseInteger: Int = {
    val value = nextTokenIntegerValue
    expect(IntegerLiteral)
    value
  }
}
