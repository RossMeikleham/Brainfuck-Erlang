%Brainfuck Interpreter
%Ross Meikleham 2015


-module(brainfuck).
-export([start/0]).
-record(state, {data_pointer, cells, ins, pc}).

% Create the state from startup and reset all memory
% locations to 0
restartInterpreter(Data) -> 
    #state{data_pointer = 0, cells = array:new(4096, {default, 0}), 
           ins = array:from_list(Data), pc = 0}.

% Decrement the data pointer
incDataPtr(St) -> 
    St#state{data_pointer = St#state.data_pointer + 1, cells = St#state.cells}.

% Increment the data pointer
decDataPtr(St) ->
    St#state{data_pointer = St#state.data_pointer - 1}.

% Increment byte at the current data pointer
incByte(St) ->
    Dp = St#state.data_pointer,
    C  = St#state.cells,
    D  = array:set(Dp, (array:get(Dp, C) + 1) rem 256, C),
    St#state{cells = D}.

% Decrement byte at the current data pointer
decByte(St) -> 
    Dp = St#state.data_pointer,
    C  = St#state.cells,
    D  = array:set(Dp, (array:get(Dp, C) - 1) rem 256, C),
    St#state{cells = D}.

% Output byte at the current data pointer
outputByte(St) ->
    Dp = St#state.data_pointer,
    Cp = St#state.cells,
    io:fwrite("~c",[array:get(Dp, Cp)]),
    St.

% Store one byte of input in the memory address of the current data pointer
storeByte(St) ->
    Char = io:get_chars("prompt>", 1),
    Dp = St#state.data_pointer,
    C = St#state.cells,
    St#state{cells = array:set(Dp, Char, C)}. 

% Returns the position in memory
% after the next right square bracket
getNextRBrace(St, Pc) ->
    case (array:get(Pc, St#state.ins) == 93) of
        true -> Pc + 1;
        false -> getNextRBrace(St, Pc + 1)
    end.

% If the byte at the data pointer is 0 then instead of moving the 
% instruction pointer forward, jump it forward to the command after that
% matching "]"        
jmpForward(St) ->
    Dp = St#state.data_pointer,
    C = St#state.cells,
    Pc = St#state.pc,
    io:fwrite("~p", 
    case (array:get(Dp, C) == 0) of
        true -> St#state{pc = getNextRBrace(St, Pc)};
        false -> St
   end.




getPreviousLBrace(St, Pc) ->
    io:fwrite(" ~p ",[Pc]),
    case (array:get(Pc, St#state.ins) == 91) of
        true -> Pc - 1;
        false -> getPreviousLBrace(St, Pc - 1)
    end.

% If the byte at the data pointe ris non zero then instead of moving
% the instruction pointer forward, jump it backward to the command
% after the matching "]"
jmpBackward(St) ->
    Dp = St#state.data_pointer,
    C = St#state.cells,
    Pc = St#state.pc,
    case (array:get(Dp, C) /= 0) of
        true -> St#state{pc = getPreviousLBrace(St, Pc)};
        false -> St
    end.


cycle(St) ->
    Pc = St#state.pc,
    NewSt = St#state{pc =  Pc + 1},
    Ins = St#state.ins,
    Op = array:get(Pc, Ins),
    
    case (Pc < array:size(St#state.ins)) of
        true -> cycle (
            case Op of
                43 -> incByte(NewSt);
                45 -> decByte(NewSt);
                62 -> incDataPtr(NewSt);
                60 -> decDataPtr(NewSt);
                46 -> outputByte(NewSt);
                91 -> jmpForward(NewSt);
                93 -> jmpBackward(NewSt);
                _ -> NewSt
            end);          
        false -> 1
    end.




start() ->
    File = case file:read_file("examples/helloworld.bf") of
        {ok, F} -> unicode:characters_to_list(F);
        {error, R} -> erlang:error(R)
    end,    
    Start = restartInterpreter(File),
    cycle(Start).
    %Next = incByte(Start),
    %outputByte(Next).
