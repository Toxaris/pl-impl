package pli

import java.io.Reader

/** $doc1 $doc2
  *
  * Each token type is implemented as a case object, see “Known
  * Subclasses” below for a list.
  *
  * @define doc1 Used by the [[Lexer lexer]] to classify
  * @define doc2 tokens in the token stream.
  */
trait TokenType

/** $doc1 “`}`” $doc2 */
case object ClosingBrace extends TokenType

/** $doc1 “`)`” $doc2 */
case object ClosingParenthesis extends TokenType

/** $doc1 “`else`” $doc2 */
case object ElseKeyword extends TokenType

/** $doc1 the end of the token stream. */
case object EndOfFile extends TokenType

/** $doc1 “`=`” $doc2 */
case object EqualsOperator extends TokenType

/** $doc1 “`extends`” $doc2 */
case object ExtendsKeyword extends TokenType

/** $doc1 identifier $doc2
  *
  * The name of the identifier is available from
  * [[Lexer.nextTokenText]].*/
case object Identifier extends TokenType

/** $doc1 “`if`” $doc2 */
case object IfKeyword extends TokenType

/** $doc1 integer literal $doc2
  *
  * The value of the integer literal is available from
  * [[Lexer.nextTokenIntegerValue]].*/
case object IntegerLiteral extends TokenType

/** $doc1 “`-`” $doc2 */
case object MinusOperator extends TokenType

/** $doc1 “`object`” $doc2 */
case object ObjectKeyword extends TokenType

/** $doc1 “`{`” $doc2 */
case object OpeningBrace extends TokenType

/** $doc1 “`(`” $doc2 */
case object OpeningParenthesis extends TokenType

/** $doc1 “`+`” $doc2 */
case object PlusOperator extends TokenType

/** $doc1 “`print`” $doc2 */
case object PrintKeyword extends TokenType

/** $doc1 “`;`” $doc2 */
case object Semicolon extends TokenType

/** $doc1 “`*`” $doc2 */
case object TimesOperator extends TokenType

/** $doc1 “`var`” $doc2 */
case object VarKeyword extends TokenType

/** $doc1 “`while`” $doc2 */
case object WhileKeyword extends TokenType

/** Factory methods for creating [[pli.Parser parsers]]. */
object Lexer {
  /** Creates a lexer for the given source code. */
  def forString(text: String): Lexer =
    new Lexer(new java.io.StringReader(text))

  /** Creates a lexer for the source code in the given file. */
  def forFile(filename: String): Lexer =
    new Lexer(new java.io.FileReader(filename))
}

/** Turns a stream of characters into a stream of tokens.
  *
  * To create a lexer, use the companion object’s [[Lexer.forFile
  * `forFile`]] or [[Lexer.forString `forString`]] methods.
  *
  * The lexer is the first main componenent in the language
  * implementation. It reads a stream of characters from a file,
  * or for testing, from a string. It produces a stream of tokens
  * which is further processed by the [[Parser parser]].
  *
  * The lexer classifies tokens according to their [[TokenType
  * token type]]. The token type of the next token in the token
  * stream is available from [[nextTokenType]].
  *
  * The lexer keeps track of line and column positions inside the
  * source code. The first character of the source code is line 1
  * and column 1. The location of the beginning of the next token
  * is available from [[startLine]] and [[startColumn]]. The
  * location of the first character after the next token is
  * available from [[currentLine]] and [[currentColumn]].
  *
  * Most token types classify a fixed string such as an operator
  * or keyword. The following token types are special:
  *
  *   - [[EndOfFile]] ─ Used for every position after
  *     the last token in the stream
  *   - [[Identifier]] ─ The name of the identifier is
  *     available from [[nextTokenText]]
  *   - [[IntegerLiteral]] ─ The value of the integer
  *     literal is available from [[nextTokenIntegerValue]]

  */
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

      case '{' =>
        skipNextCodepoint()
        nextTokenType = OpeningBrace

      case '}' =>
        skipNextCodepoint()
        nextTokenType = ClosingBrace

      case ';' =>
        skipNextCodepoint()
        nextTokenType = Semicolon

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
            case "else" => ElseKeyword
            case "extends" => ExtendsKeyword
            case "if" => IfKeyword
            case "object" => ObjectKeyword
            case "print" => PrintKeyword
            case "var" => VarKeyword
            case "while" => WhileKeyword
            case "abstract" | "case" | "catch" | "class" | "def"
               | "do" | "false" | "final" | "finally" | "for"
               | "forSome" | "implicit" | "import" | "lazy"
               | "match" | "new" | "null" | "override"
               | "package" | "private" | "protected" | "return"
               | "sealed" | "super" | "this" | "throw" | "trait"
               | "try" | "true" | "type" | "val" | "with" | "yield" =>
              throw new Error("Unsupported keyword '" + nextTokenText + "'")
            case _ => Identifier
          }

     case _ if atOperatorCharacter =>
        readNextCodepoint()
        while (atOperatorCharacter) {
          readNextCodepoint()
        }
        nextTokenType =
          nextTokenText.toString match {
            case "+" => PlusOperator
            case "-" => MinusOperator
            case "*" => TimesOperator
            case "=" => EqualsOperator
            case _ => throw new Error("Unknown token '" + nextTokenText + "'")
          }

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

  /** Determine whether the given code point is an operator
    * character according to the Scala Language Specification,
    * Chapter 1.
    */
  def isOperatorCharacter(codepoint: Int): Boolean = {
    val category = Character.getType(codepoint)

    codepoint == 0x0021 ||
    codepoint == 0x0023 ||
    codepoint == 0x0025 ||
    codepoint == 0x0026 ||
    codepoint == 0x002A ||
    codepoint == 0x002B ||
    codepoint == 0x002D ||
    codepoint == 0x002F ||
    codepoint == 0x003A ||
    codepoint == 0x003C ||
    codepoint == 0x003D ||
    codepoint == 0x003E ||
    codepoint == 0x003F ||
    codepoint == 0x0040 ||
    codepoint == 0x005C ||
    codepoint == 0x003E ||
    codepoint == 0x007C ||
    codepoint == 0x007E ||
    category == Character.MATH_SYMBOL ||
    category == Character.OTHER_SYMBOL
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

  /** Determine whether the next code point in the input is an
    * operator character according to the Scala Language
    * Specification, Chapter 1.
    */
  def atOperatorCharacter: Boolean =
    isOperatorCharacter(nextCodepoint)

  /** Determine whether the next token has the specified type,
    * but don't process that token.
    */
  def at(tokenType: TokenType): Boolean =
    nextTokenType == tokenType

  /** Determine whether the next token has the specified type,
    * and process it if it does.
    */
  def check(tokenType: TokenType): Boolean = {
    val result = at(tokenType)
    if (result) {
      readNextToken()
    }
    result
  }

  /** Process the next token if it has the specified type,
    * and throw an error if it doesn't.
    */
  def expect(tokenType: TokenType) {
    if (!check(tokenType)) {
      throw new Error("Unexpected " + nextTokenType)
    }
  }
}
