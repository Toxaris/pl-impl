/** An implementation of a small subset of Scala.
  *
  * The code in this package is part of the course material for
  * the course “Implementation of Programming Languages” at
  * Tübingen University, Germany.
  *
  * Main components:
  *
  *   - [[Lexer]] ─ Turns a stream of characters into a stream of tokens
  *   - [[Parser]] ─ Turns a stream of tokens into an abstract syntax tree
  *   - [[Interpreter]] ─ Executes abstract syntax trees directly
  *   - [[CodeGenerator]] ─ Generates simple bytecode from abstract syntax trees
  *   - [[VM]] ─ Executes bytecode
  *
  * Helper components:
  *
  *   - [[Pretty]] ─ Turns an abstract syntax tree back into a string
  *
  * Main data structures:
  *
  *   - [[TokenType]] ─ Types of tokens in the token stream
  *   - [[ASTNode]] ─ Nodes of the abstract syntax tree
  *   - [[Table]] ─ Scoped associated array, used for environments and symbol tables
  *   - [[Bytecode]] ─ Builder for bytecode arrays, with support for backpatching
  *   - [[Opcode]] ─ Opcodes and documentation for bytecode instructions
  */
package object pli {
}
