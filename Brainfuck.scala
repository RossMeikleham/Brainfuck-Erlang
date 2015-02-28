
// Brainfuck Interpreter written in Scala
object Brainfuck {

    class CPU {  
      var cells:Array[Byte] = new Array[Byte](0x4000) // Memory cells
      var pc = 0 // Program Counter
      var dp = 0 // Data Pointer

      def getOp() : Char = { 
        cells(pc).toChar 
      }
    }

    def main(args: Array[String]) {
        
        var fileName = args(0)         
        println("Hello World!\n" + fileName); 
    }

    def run(cpu: CPU) : CPU  = cpu.getOp() match {
      case '>' => run(cpu)
      case '<' => run(cpu)
      case '+' => run(cpu)
      case '-' => run(cpu)
      case '[' => run(cpu)
      case ']' => run(cpu)
      case ',' => run(cpu)
      case '.' => run(cpu)
      case  _  => cpu

    }
    
    
}

