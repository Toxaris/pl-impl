package pli

class ParserSpec extends Spec with ASTMatchers {
  "The parser" when "looking for expressions" should
  "accept positive integer literals" in {
    val result = Parser.forString("123").parseExpression
    result should be (a [Literal])
    result should have ('value(123))
  }

  it should "accept variable references" in {
    val result = Parser.forString("hello").parseExpression
    result should be (a [Variable])
    result should have ('name("hello"))
  }

  it should "accept addition" in {
    val result = Parser.forString("1 + 2").parseExpression
    result should be (an [Addition])
  }

  it should "accept subtraction" in {
    val result = Parser.forString("1 - 2").parseExpression
    result should be (a [Subtraction])
  }

  it should "accept multiplication" in {
    val result = Parser.forString("1 * 2").parseExpression
    result should be (a [Multiplication])
  }

  it should "accept parentheses" in {
    val result = Parser.forString("(42)").parseExpression
    result should be (a [Literal])
  }

  "Addition " should "be left-associative" in {
    val result = Parser.forString("1 + 2 + 3").parseExpression
    result should be (an [Addition])
    inside(result) { case result: Addition =>
      result.lhs should be (an [Addition])
      result.rhs should be (a [Literal])
    }
  }

  it should "bind as strong as subtraction" in {
    val result = Parser.forString("1 + 2 - 3").parseExpression
    result should be (a [Subtraction])
  }

  "Subtraction" should "be left-associative" in {
    val result = Parser.forString("1 - 2 - 3").parseExpression
    result should be (a [Subtraction])
    inside(result) { case result: Subtraction =>
      result.lhs should be (a [Subtraction])
      result.rhs should be (a [Literal])
    }
  }

  it should "bind as strong as addition" in {
    val result = Parser.forString("1 - 2 + 3").parseExpression
    result should be (an [Addition])
  }

  "Multiplication" should "be left-associative" in {
    val result = Parser.forString("1 * 2 * 3").parseExpression
    result should be (a [Multiplication])
    inside(result) { case result: Multiplication =>
      result.lhs should be (a [Multiplication])
      result.rhs should be (a [Literal])
    }
  }

  it should "bind stronger than addition" in {
    val result = Parser.forString("1 + 2 * 3").parseExpression
    result should be (an [Addition])
  }

  it should "bind stronger than subtraction" in {
    val result = Parser.forString("1 - 2 * 3").parseExpression
    result should be (a [Subtraction])
  }

  "Parentheses" should "group expressions" in {
    val result = Parser.forString(" 1 * (2 + 3)").parseExpression
    result should be (a [Multiplication])
    inside(result) { case result: Multiplication =>
      result.lhs should be (a [Literal])
      result.rhs should be (an [Addition])
    }
  }

  "The parser" when "looking for statements" should
  "accept variable declarations" in {
    val result = Parser.forString("var answer = 42;").parseStatement
    result should be (a [Var])
  }

  it should "accept assignments" in {
    val result = Parser.forString("answer = 42;").parseStatement
    result should be (an [Assignment])
  }

  it should "accept print statements" in {
    val result = Parser.forString("print(42);").parseStatement
    result should be (a [Print])
  }

  it should "accept empty blocks" in {
    val result = Parser.forString("{}").parseStatement
    result should be (a [Block])
    inside(result) { case result: Block =>
      result.body should be (empty)
    }
  }

  it should "accept non-empty blocks" in {
    val result = Parser.forString("{print(1); print(2);}").parseStatement
    result should be (a [Block])
    inside(result) { case result: Block =>
      result.body should have (length(2))
    }
  }

  it should "accept while loops" in {
    val result = Parser.forString("while (1) print(2);").parseStatement
    result should be (a [While])
  }

  it should "accept if-then-else statements" in {
    val result = Parser.forString("if (1) print(2); else print(3);").parseStatement
    result should be (an [If])
  }

  it should "accept if-then statements" in {
    val result = Parser.forString("if (1) print(2);").parseStatement
    result should be (an [If])
    inside(result) { case result: If =>
      result.elseBranch should be (a [Block])
      inside (result.elseBranch) { case elseBranch: Block =>
        elseBranch.body should be (empty)
      }
    }
  }

  it should "associate dangling else branches to the nearest if" in {
    val result = Parser.forString("if (1) if (2) {} else print(3);").parseStatement
    result should be (an [If])
    inside(result) { case result: If =>
      result.elseBranch should be (a [Block])
      result.thenBranch should be (an [If])
      inside(result.thenBranch) { case thenBranch: If =>
        thenBranch.elseBranch should be (a [Print])
      }
    }
  }

  "The parser" when "looking for a program" should
  "accept programs with empty body" in {
    val result = Parser.forString("object foo extends bar {}").parseProgram
    result.body should be (empty)
  }

  it should "accept programs with non-empty body" in {
    val result = Parser.forString("object foo extends bar {print(1); print(2);}").parseProgram
    result.body should have (length(2))

  }

}
