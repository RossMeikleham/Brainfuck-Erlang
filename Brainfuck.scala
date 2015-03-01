import java.nio.file.{Files, Paths}

// Brainfuck Interpreter written in Scala
object Brainfuck {

    class CPU(instructions : Array[Byte]) {  
      var pc = -1  // Program Counter
      var dp = 0  // Data Pointer
      var cells : Array[Byte] = new Array[Byte](0x4000)

      def getOp() : Char = { 
        instructions(pc).toChar 
      }

      // Check if PC is out of bounds
      def outOfBounds : Boolean = {
        pc < 0 || pc >= instructions.length
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
        cells(dp) = (cells(dp).toInt + 1).toByte
        this
      } 

      //Decrement Byte at the Data Pointer
      def decByte() : CPU = {
        cells(dp) = (cells(dp).toInt - 1).toByte
        this
      }

      def getByte() : CPU = {
        print("prompt>")
        cells(dp) = readChar.toByte
        this 
      }

      def outputByte() : CPU = {
        print(cells(dp).toChar)  
        this
      }

      // Set the PC after the next matching "]" if
      // value at the data pointer is equal to 0
      def jumpForwardZ() : CPU = {
        cells(dp) match {
          case 0 => jumpForward(pc + 1, 0)
          case _ => this
        }

        def jumpForward(tempPc: Int, stackLevel: Int) : CPU = {
          instructions(tempPc).toChar match {
            case '[' => jumpForward(tempPc + 1, stackLevel + 1)
            case ']' => stackLevel match {
                          case 0 => {this.pc = tempPc - 1
                                     this
                                    }
                          case _ => jumpForward(tempPc + 1, stackLevel - 1)
                        }
            case _   => jumpForward(tempPc + 1, stackLevel)
          }
        }

        this
      }

      // Set the PC before the previous matching "[" if
      // value at the data pointer is not equal to 0
      def jumpBackNZ() : CPU = {
        cells(dp) match {
          case 0 => this
          case _ => jumpBack(pc - 1, 0)
        }
                 
        
        def jumpBack(tempPc: Int, stackLevel: Int) : CPU = {
          instructions(tempPc).toChar match {
            case ']' => jumpBack(tempPc - 1, stackLevel + 1)
            case '[' => stackLevel match {
                          case 0 => {this.pc = tempPc - 1
                                     this
                                    }
                          case _ => jumpBack(tempPc - 1, stackLevel - 1)
                        }
            case _  => jumpBack(tempPc - 1, stackLevel)
          }   
        }

        this
      }

    }

    def main(args: Array[String]) {
        
        val fileName = args(0)   
        val file = Files.readAllBytes(Paths.get(fileName))      
        var cpu = new CPU(file.filter(x => "<>+-[],.".indexOf(x.toChar) != -1 ))
        run(cpu)   
        println("") 
    }

    // Run the interpreter
    def run(cpu: CPU) : CPU  = {

      val newCPU = cpu.incPC()
      
      if (!newCPU.outOfBounds) {
        newCPU.getOp() match {
          case '>' => run(newCPU.incDP())
          case '<' => run(newCPU.decDP())
          case '+' => run(newCPU.incByte())
          case '-' => run(newCPU.decByte())
          case '[' => run(newCPU.jumpForwardZ)
          case ']' => run(newCPU.jumpBackNZ())
          case ',' => run(newCPU.getByte())
          case '.' => run(newCPU.outputByte())
          case  _  => run(newCPU)
        }
      } else {
        newCPU
      }
    }   
    
}

