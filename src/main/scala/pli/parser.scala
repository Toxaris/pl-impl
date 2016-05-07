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

/** Turns a stream of tokens into an abstract syntax tree.
  *
  * To create a parser, use the companion object’s
  * [[Parser#forLexer `forLexer`]], [[Parser#forFile `forFile`]],
  * or [[Parser#forString `forString`]] methods.
  *
  * The parser is the second main component in the language
  * implementation. It reads a stream of [[pli.TokenType tokens]]
  * from the [[pli.Lexer lexer]]. It produces an [[pli.ASTNode
  * abstract syntax tree]] which is further processed by the
  * other components (see [[pli overview]]).
  *
  * The parser has one method `parseN` for each nonterminal
  * symbol `N` in the grammar´:
  *
  *   - [[parseProgram]]
  *   - [[parseStatement]]
  *   - [[parseExpression]]
  *   - [[parseName]]
  *   - [[parseInteger]]
  *
  * Additional `parseN` methods are necessary for helper
  * nonterminal symbols to implement operator precedence:
  *
  *   - [[parseSummand]]
  *   - [[parseFactor]]
  */
class Parser(lexer: Lexer) {
  import lexer._

  /** Parses programs. */
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

  /** Parses sequence of statements in braces. */
  def parseBody: Seq[Statement] = {
    expect(OpeningBrace)
    val body = Seq.newBuilder[Statement]
    while (!check(ClosingBrace)) {
      body += parseStatement
    }
    body.result
  }

  /** Parses statements. */
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
      val body = parseBody
      While(condition, body)
    } else if (check(IfKeyword)) {
      expect(OpeningParenthesis)
      val condition = parseExpression
      expect(ClosingParenthesis)
      val thenBranch = parseBody
      if (check(ElseKeyword)) {
        val elseBranch = parseBody
        If(condition, thenBranch, elseBranch)
      } else {
        If(condition, thenBranch, Seq())
      }
    } else if (at(OpeningBrace)) {
      val body = parseBody
      Block(body)
    } else {
      throw new Error("unexpected " + nextTokenType)
    }
  }

  /** Parses expressions. */
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

  /** Parses expression that can be the argument of a “`+`” or
    * “`-`” operator.*/
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

  /** Parses expression that can be the argument of a “`*`”
    * operator.*/
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

  /** Parses names.*/
  def parseName: String = {
    val text = nextTokenText.toString
    expect(Identifier)
    text
  }

  /** Parses literal integers. */
  def parseInteger: Int = {
    val value = nextTokenIntegerValue
    expect(IntegerLiteral)
    value
  }
}
