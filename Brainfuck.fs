(*Brainfuck interprerter in F#*)

open System
open System.IO

type CPU = {
    instructions : byte array;
    cells        : byte array;
    pc           : uint32;
    dp           : uint32;
}
   

let createCPU (ins : byte array) : CPU = 
    {instructions = ins; cells = Array.zeroCreate 30000; pc = 0ul; dp = 0ul}    


// Jump forwards if cell at the current data pointer is 0
let jumpForward (cpu : CPU) : CPU = 
     
    let rec jumpForwards (i : uint32) (s : uint32) (cpu : CPU) : CPU =        
         match char(cpu.instructions.[int(i)]) with 
         | '[' -> jumpForwards (i + 1ul) (s + 1ul) cpu
         | ']' -> if s = 0ul 
                      then {cpu with pc = i + 1ul} 
                      else jumpForwards (i + 1ul) (s - 1ul) cpu
         | _   -> jumpForwards (i + 1ul) s cpu 

    if cpu.cells.[int32(cpu.dp)] <> 0uy
        then {cpu with pc = cpu.pc + 1ul}
        else jumpForwards (cpu.pc + 1ul) 0ul cpu


// Jump backwards if cell at the current data pointer is not 0    
let jumpBackward (cpu : CPU) : CPU = 
     
    let rec jumpBackwards (i : uint32) (s : uint32) (cpu : CPU) : CPU =        
         match char(cpu.instructions.[int(i)]) with 
         | ']' -> jumpBackwards (i - 1ul) (s + 1ul) cpu
         | '[' -> if s = 0ul 
                      then {cpu with pc = i} 
                      else jumpBackwards (i - 1ul) (s - 1ul) cpu
         | _   -> jumpBackwards (i - 1ul) s cpu 

    if cpu.cells.[int32(cpu.dp)] = 0uy
        then {cpu with pc = cpu.pc + 1ul}
        else jumpBackwards (cpu.pc - 1ul) 0ul cpu


// Execute for one cycle
let run (cpu : CPU) : CPU = 
    let incPC cpu = {cpu with pc = cpu.pc + 1ul}

    match char(cpu.instructions.[int(cpu.pc)]) with
    | '+' -> cpu.cells.[int32(cpu.dp)] <- cpu.cells.[int32(cpu.dp)] + 1uy
             incPC cpu

    | '-' -> cpu.cells.[int32(cpu.dp)] <- cpu.cells.[int32(cpu.dp)] - 1uy
             incPC cpu

    | '.' -> let c = char(cpu.cells.[int32(cpu.dp)])
             printf "%c" c
             incPC cpu

    | ',' -> let str = Console.ReadLine()
             cpu.cells.[int32(cpu.dp)] <- byte(str.[0])
             incPC cpu
          
    | '<' -> let cpu' = {cpu with dp = cpu.dp - 1ul}
             incPC cpu'

    | '>' -> let cpu' = {cpu with dp = cpu.dp + 1ul}
             incPC cpu'

    | '[' -> jumpForward cpu

    | ']' -> jumpBackward cpu

    | _ -> cpu


// Main execution loop
let rec execute (length : uint32) (cpu : CPU) =
    if cpu.pc < length
        then
            execute length (run cpu)


[<EntryPoint>]     
let main argv =
    let args = Environment.GetCommandLineArgs() 
    match (args |> List.ofSeq) with
    | pName :: fName :: [] ->
        printfn "%s" fName
        // Read in file, filter out all characters which aren't instructions, build the initial CPU
        // and run
        let file = System.Text.Encoding.ASCII.GetBytes(File.ReadAllText fName)
        let containsIns c = Array.exists (fun elem -> elem = c) "[],.<>+-"B 
        let ins = file |> Array.filter containsIns 
        let cpu = createCPU ins
        execute (uint32(Array.length ins)) cpu
        0


    | pName :: [] -> 
        printfn "Usage: %s \"filename\"" pName
        0

    | _ -> 
        printfn "Error reading command line args"
        0
