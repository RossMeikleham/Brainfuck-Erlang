
// Brainfuck Interpreter written in Scala
object Brainfuck {

    class CPU {  
      var cells: Array[Byte] = new Array[Byte](0x4000) // Memory cells
      var pc = 0  // Program Counter
      var dp = 0  // Data Pointer
      
      def CPU(program : Array[Byte]) {
        cells = program
      }

      def getOp() : Char = { 
        cells(pc).toChar 
      }
      
      // Increment Program Counter
      def incPC() : CPU = {
        pc += 1
        this
      }

      // Increment Data Pointer
      def incDP() : CPU = {
        dp += 1
        this
      }

      // Decrement Data Pointer
      def decDP() : CPU = {
        dp -= 1
        this
      }
      
      //Increment Byte at the Data Pointer
      def incByte() : CPU = {
        cells(dp) = cells(dp) + 1
        this
      } 

      //Decrement Byte at the Data Pointer
      def decByte() : CPU = {
        cells(dp) = cells(dp) - 1
        this
      }



    }

    def main(args: Array[String]) {
        
        val fileName = args(0)         
        println("Hello World!\n" + fileName);
         
    }

    def run(cpu: CPU) : CPU  = {

      val newCPU = cpu.incPC()

      cpu.getOp() match {
        case '>' => run(newCPU.incDP())
        case '<' => run(newCPU.decDP())
        case '+' => run(newCPU.incByte())
        case '-' => run(newCPU.decByte())
        case '[' => run(cpu)
        case ']' => run(cpu)
        case ',' => run(cpu)
        case '.' => run(cpu)
        case  _  => cpu

    }
    }
    
    
}

