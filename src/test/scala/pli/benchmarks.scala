package pli

import org.scalameter.api._

object Benchmarks extends Bench.LocalTime {
  val ns = Gen.range("n")(10000, 100000, 10000)

  val texts = for {
    n <- ns
  } yield (
    s"""
     object foo extends bar {
       var n = $n;
       var x = 1;
       while (n) {
         n = n - 1;
         x = x + x;
       }
     }
     """)

  val asts = for {
    text <- texts
  } yield (Parser.forString(text).parseProgram)

  val bytecodes = for {
    ast <- asts
  } yield {
    val codegen = new CodeGenerator()
    codegen.generate(ast)
    codegen.bytecode.exit()
    codegen.bytecode.result
  }

  performance of "VM" in {
    performance of "power-of-two" in {
      using(bytecodes) in {
        bytecode => new VM(bytecode).run()
      }
    }
  }

  performance of "Interpreter" in {
    performance of "power-of-two" in {
      using(asts) in {
        ast => new Interpreter().run(ast)
      }
    }
  }
}
