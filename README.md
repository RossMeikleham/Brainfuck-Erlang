#  Functional Brainfuck Interpreters

Some Brainfuck interpreters written in Functional programming languages.

Brainfuck is an esoteric language, more information can be found on it [here](http://en.wikipedia.org/wiki/Brainfuck)

# Erlang
-  From the command line enter the erlang shell by typing `erl`
-  Compile and load the Brainfuck module with `c(brainfuck).`
-  Interpret a Brainfuck file with`brainfuck:start([FILENAME]).`

# Haskell
- From the command line enter `ghci`
- then `:l Brainfuck`
- Interpret a Brainfuck file with `runBF "FileName"`

# Scala
- From the command line enter `scala Brainfuck.scala "FileName"`

# F&#35;
- From the command line run `fsc Brainfuck.fs` to compile to an executable
- `./Brainfuck "FileName"` to run
