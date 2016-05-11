package pli

// To maximize reuse, the documentation of the abstract syntax
// tree nodes is structure by level:
//
//   - $doc1 is the documentation of ASTNode at the root of the
//     hierarchy,
//
//   - $doc2 is the documentation for intermediate traits that
//     classify all nodes belonging to the same nonterminal, and
//
//   - $doc3 is the documentation for case classes at the bottom
//     of the hierarchy.

/** $doc1.
  *
  * The abstract syntax tree is one of the main data structures
  * of the language implementation. It is produced by the
  * [[Parser parser]] and consumed by many of the other
  * components (see [[pli overview]]).
  *
  * $subclasses They are further classified by nonterminal by
  * intermediate traits in the inheritance hierarchy.
  *
  * @define doc1
  * Nodes of the [[ASTNode abstract syntax tree]]
  * @define doc2
  * $doc1 $nonterminal
  * @define doc3
  * $doc2 of the form “$phrase”
  * @define nonterminal
  * @define subclasses
  * The concrete nodes $nonterminal of the abstract syntax tree are
  * implemented by case classes extending this trait, see “Known
  * Subclasses” below for a list.
  */
trait ASTNode

/** $doc3.
  *
  * @define nonterminal for programs
  * @define phrase `object` [[name]] `extends` [[superclass]] `{` [[body]] `}`
  */
case class Program(name: String, superclass: String, body: Seq[Statement]) extends ASTNode

/** $doc2.
  *
  * $subclasses
  *
  * @define nonterminal for statements
  */
trait Statement extends ASTNode

/** $doc3.
  * @define phrase `var` [[name]] `=` [[value]]
  */
case class Var(name: String, value: Expression) extends Statement

/** $doc3.
  * @define phrase [[name]] `=` [[value]]
  */
case class Assignment(name: String, value: Expression) extends Statement

/** $doc3.
  * @define phrase `print` `(` [[value]] `)`
  */
case class Print(value: Expression) extends Statement

/** $doc3.
  * @define phrase `while` `(` [[condition]] `)` [[body]]
  */
case class While(condition: Expression, body: Seq[Statement]) extends Statement

/** $doc3.
  *
  * If there is no `else` clause in the source code, the
  * [[elseBranch]] will be an empty [[Block]] as if “`else {}`”
  * would have been in the source code.
  *
  * @define phrase `if` `(` [[condition]] `)` [[thenBranch]]” or
  * “`if` `(` [[condition]] `)` [[thenBranch]] `else`
  * [[elseBranch]]`
  */
case class If(condition: Expression, thenBranch: Seq[Statement], elseBranch: Seq[Statement]) extends Statement

/** $doc2.
  * @define phrase `{` [[body]] `}`
  */
case class Block(body: Seq[Statement]) extends Statement

/** $doc2.
  *
  * $subclasses
  *
  * @define nonterminal for expressions
  */
trait Expression extends ASTNode

/** $doc3.
  * @define phrase [[name]]
  */
case class Variable(name: String) extends Expression

/** $doc3.
  * @define phrase [[value]]
  */
case class Literal(value: Int) extends Expression

/** $doc3.
  * @define phrase [[lhs]] `+` [[rhs]]
  */
case class Addition(lhs: Expression, rhs: Expression) extends Expression

/** $doc3.
  * @define phrase [[lhs]] `-` [[rhs]]
  */
case class Subtraction(lhs: Expression, rhs: Expression) extends Expression

/** $doc3.
  * @define phrase [[lhs]] `*` [[rhs]]
  */
case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
