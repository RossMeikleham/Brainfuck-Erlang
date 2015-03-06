(*Brainfuck interprerter in F#*)

type cpu =
    { instructions : byte array;
      cells        : byte array;
      pc           : uint32 array;
      dp           : uint32 array;
    }

let main = printf "Hello World!"
