-module(day5).
-compile('export_all').

readmem(TargetPos, Allcodes) ->
    lists:nth(TargetPos+1, Allcodes).

compute(1, Term1, Term2) ->
    %% ADDITION operator
    Term1 + Term2;
compute(2, Term1, Term2) ->
    %% MULTIPLY operator
    Term1 * Term2;
compute(7, Term1, Term2) ->
    %% LESS THAN operator
    if Term1 < Term2 -> 1;
       true          -> 0
    end;
compute(8, Term1, Term2) ->
    %% EQUALITY operator
    if Term1 =:= Term2 -> 1;
       true          -> 0
    end.

replacecode(CurPos, Opcodes, TargetPos, _Value) when TargetPos < CurPos ->
    Opcodes;
replacecode(CurPos, Opcodes, TargetPos, Value) ->
    Offset = TargetPos - CurPos,
    {List1, List2} = lists:split(Offset, Opcodes),
    List1 ++ [Value | tl(List2)].

splitop(OpAM) ->
    {OpAM rem 100, (OpAM div 100) rem 10, (OpAM div 1000) rem 10, (OpAM div 10000) rem 10}.

getparam(Param, _Codes, 1) ->  %% immediate mode
    Param;
getparam(Param, Codes, 0) ->   %% position mode
    readmem(Param, Codes).

seek(Codes, Target) ->
    seek(Codes, Target, 0).
seek(Codes, Target, Target) ->
    Codes;
seek([_ | Rest], Target, Current) ->
    seek(Rest, Target, Current+1).

runcode(Opcodes, Input, Debug) ->
    Allcodes = runcode(Opcodes, Input, 0, Opcodes),
    if Debug -> Allcodes;
       true  -> ok
    end.
runcode([99 | _], _Input, _Position, Allcodes) ->
    %% HALT instruction
    Allcodes;
runcode([N, T | Rest], Input, Position, Allcodes) when (N rem 100) == 3 ->
    %% INPUT instruction, take Input and store at target address
    NewPosition = Position + 2,
    NewOpcodes  = replacecode(NewPosition, Rest, T, Input),
    NewAllcodes = replacecode(0, Allcodes, T, Input),
    runcode(NewOpcodes, Input, NewPosition, NewAllcodes);
runcode([N, P | Rest], Input, Position, Allcodes) when (N rem 100) == 4 ->
    %% OUTPUT instruction, read parameter and print it
    NewPosition = Position + 2,
    Mode = N div 100,
    Value = getparam(P, Allcodes, Mode),
    io:format("Output: ~p~n", [Value]),
    runcode(Rest, Input, NewPosition, Allcodes);    
runcode([N, P1, P2 | Rest], Input, Position, Allcodes) when (N rem 100) == 5 ->
    %% JUMP, if parameter is non-zero
    {5, Mode1, Mode2, 0} = splitop(N),
    Value = getparam(P1, Allcodes, Mode1),
    {NewPosition, NewOpcodes} = if (Value =/= 0) ->
					T = getparam(P2, Allcodes, Mode2),
					{T, seek(Allcodes, T)};
				   true -> {Position + 3, Rest}
				end,
    runcode(NewOpcodes, Input, NewPosition, Allcodes);
runcode([N, P1, P2 | Rest], Input, Position, Allcodes) when (N rem 100) == 6 ->
    %% JUMP, if parameter _is_ zero
    {6, Mode1, Mode2, 0} = splitop(N),
    Value = getparam(P1, Allcodes, Mode1),
    {NewPosition, NewOpcodes} = if (Value == 0) ->
					T = getparam(P2, Allcodes, Mode2),
					{T, seek(Allcodes, T)};
				   true -> {Position + 3, Rest}
				end,
    runcode(NewOpcodes, Input, NewPosition, Allcodes);
runcode([OpAndModes, P1, P2, T | Rest], Input, Position, Allcodes) ->
    {Op, Mode1, Mode2, 0} = splitop(OpAndModes), %% target is always position mode
    Term1 = getparam(P1, Allcodes, Mode1),
    Term2 = getparam(P2, Allcodes, Mode2),
    Result = compute(Op, Term1, Term2),
    NewPosition = Position + 4,
    NewOpcodes  = replacecode(NewPosition, Rest, T, Result),
    NewAllcodes = replacecode(0, Allcodes, T, Result),
    runcode(NewOpcodes, Input, NewPosition, NewAllcodes).

go() ->
    Input = 1,
    go(Input).
go(Input) ->
    {ok, [Opcodes]} = file:consult("input.txt"),
    runcode(Opcodes, Input, false).
