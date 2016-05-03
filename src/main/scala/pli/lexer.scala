package pli

import java.io.Reader

trait TokenType

case object ClassKeyword extends TokenType
case object ClosingParenthesis extends TokenType
case object DefKeyword extends TokenType
case object EqualsOperator extends TokenType
case object Identifier extends TokenType
case object IfKeyword extends TokenType
case object IntegerLiteral extends TokenType
case object MinusEqualsOperator extends TokenType
case object MinusOperator extends TokenType
case object ObjectKeyword extends TokenType
case object OpeningParenthesis extends TokenType
case object PlusEqualsOperator extends TokenType
case object PlusOperator extends TokenType
case object TimesEqualsOperator extends TokenType
case object TimesOperator extends TokenType
case object TraitKeyword extends TokenType
case object WhileKeyword extends TokenType
case object EndOfFile extends TokenType

object Lexer {
  def forString(text: String): Lexer =
    new Lexer(new java.io.StringReader(text))
}

class Lexer(input: Reader) {
  // invariants:
  //
  //  - position of input reader is always two ahead of current logical position
  //    except at end of file
  //  - nextCodepoint and nextNextCodepoint contain the characters
  //    between logical position and input reader position
  //    except at end of file, when they are both -1

  /** next code point in input string, after current position */
  var nextCodepoint = input.read()

  /** next code point after the next code point after the current position */
  var nextNextCodepoint = readNotFirstCodepoint()

  // invariants:
  //
  //  - currentLine and currentColumn is position of next character after current logical position in input string
  //  - startLine and startColumn is position of next token after current position in token stream
  var currentLine = 1
  var currentColumn = 1
  var startLine = currentLine
  var startColumn = currentColumn

  // invariants:
  //
  //  - nextTokenText contains text of next identifier in token stream, if next token is identifier
  //  - nextTokenIntegerValue contains integer value of next integer literal in token stream, if next token is integer literal
  //  - nextTokenTYpe is type of next token in token stream
  val nextTokenText = new StringBuilder
  var nextTokenIntegerValue = 0
  var nextTokenType = readNextToken()

  def readNotFirstCodepoint() =
    if (atEndOfFile()) -1 else input.read()

  def skipNextCodepoint() {
    if (atEndOfLine()) {
      currentLine += 1
      currentColumn = 1
    } else {
      currentColumn += 1
    }

    nextCodepoint = nextNextCodepoint
    nextNextCodepoint = readNotFirstCodepoint()
  }

  def readNextCodepoint() {
    registerCodepoint(nextCodepoint)
    skipNextCodepoint()
  }

  def registerCodepoint(codepoint: Int) {
    nextTokenText.append(codepoint.asInstanceOf[Char])
  }

  def atEndOfLine(): Boolean =
    nextCodepoint == '\n' || nextCodepoint == '\r'

  def atEndOfFile(): Boolean =
    nextCodepoint == -1

  def skipWhitespaceAndComments() {
    while (true) {
      nextCodepoint match {
        case '\u0020' | '\u0009' | '\u000D' | '\u000A' =>
          skipNextCodepoint()
        case '/' => nextNextCodepoint match {
          case '/' => {
            // we could remove the following two skipNextCodepoint
            // without changing the overall semantics, but we
            // leave them in to keep the coding style consistent
            // with the case for block comments below.
            skipNextCodepoint() // skip over first /
            skipNextCodepoint() // skip over second /
            while (!(atEndOfLine() || atEndOfFile())) {
              skipNextCodepoint()
            }
          }
          case '*' => {
            skipNextCodepoint() // skip over /
            skipNextCodepoint() // skip over *
            while (!(nextCodepoint == '*' && nextNextCodepoint == '/')) {
              if (atEndOfFile())
                throw new Error("end of file during block comment")
              skipNextCodepoint()
            }
            skipNextCodepoint() // skip over *
            skipNextCodepoint() // skip over /
          }
          case _ => return
        }
        case _ => return
      }
    }
  }

  def readNextToken(): TokenType = {
    skipWhitespaceAndComments();

    startLine = currentLine
    startColumn = currentColumn
    nextTokenText.clear()

    nextCodepoint match {
      case -1 =>
        nextTokenType = EndOfFile

      case '(' =>
        skipNextCodepoint()
        nextTokenType = OpeningParenthesis

      case ')' =>
        skipNextCodepoint()
        nextTokenType = ClosingParenthesis

      case _ if atDigit =>
        readNextCodepoint()
        while (atDigit) {
          readNextCodepoint()
        }
        nextTokenIntegerValue =
          Integer.valueOf(nextTokenText.toString)
        nextTokenType = IntegerLiteral

      case _ if atLetter =>
        readNextCodepoint()
        while (atLetter || atDigit) {
          readNextCodepoint()
        }
        nextTokenType =
          nextTokenText.toString match {
            case "class" => ClassKeyword
            case "def" => DefKeyword
            case "if" => IfKeyword
            case "object" => ObjectKeyword
            case "trait" => TraitKeyword
            case "while" => WhileKeyword
            // TODO: throw error on unknown keywords
            case _ => Identifier
          }

      // TODO: accept operators
      // TODO: throw error on unknown operators
      case _ =>
        throw new Error("unknown token")
    }
    nextTokenType
  }

  /** Determine whether the given code point is a letter according
    * to the Scala Language Specification, Chapter 1.
    */
  def isLetter(codepoint: Int): Boolean = {
    val category = Character.getType(codepoint)

    category == Character.LOWERCASE_LETTER ||
    category == Character.UPPERCASE_LETTER ||
    category == Character.TITLECASE_LETTER ||
    category == Character.LETTER_NUMBER ||
    codepoint == 0x0024 ||
    codepoint == 0x005F
  }

  /** Determine whether the given code point is a digit according
    * to the Scala Language Specification, Chapter 1.
    */
  def isDigit(codepoint: Int): Boolean =
    codepoint >= 0x0030 && codepoint <= 0x0039

  /** Determine whether a given code point can be an operator character
    * according to the Scala Language Specification, Chapter 1
    */

  def atLetter: Boolean =
    isLetter(nextCodepoint)

      /** Determine whether the next code point in the input is a digit
    * according to the Scala Language Specification, Chapter 1.
    */
  def atDigit: Boolean =
    isDigit(nextCodepoint)
}
