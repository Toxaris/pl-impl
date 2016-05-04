package pli

trait ASTNode

case class Program(name: String, superclass: String, body: Seq[Statement]) extends ASTNode

trait Statement extends ASTNode
case class Var(name: String, value: Expression) extends Statement
case class Assignment(name: String, value: Expression) extends Statement
case class Print(value: Expression) extends Statement
case class While(condition: Expression, body: Statement) extends Statement
case class If(condition: Expression, thenBranch: Statement, elseBranch: Statement) extends Statement
case class Block(statements: Seq[Statement]) extends Statement

trait Expression extends ASTNode
case class Variable(name: String) extends Expression
case class Literal(value: Int) extends Expression
case class Addition(lhs: Expression, rhs: Expression) extends Expression
case class Subtraction(lhs: Expression, rhs: Expression) extends Expression
case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
