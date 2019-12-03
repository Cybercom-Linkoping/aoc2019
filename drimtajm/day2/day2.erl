-module('day2').
-compile('export_all').

readmem(TargetPos, Allcodes) ->
    lists:nth(TargetPos+1, Allcodes).

compute(1, Term1, Term2) ->
    Term1 + Term2;
compute(2, Term1, Term2) ->
    Term1 * Term2.

replacecode(CurPos, Opcodes, TargetPos, _Value) when TargetPos < CurPos ->
    Opcodes;
replacecode(CurPos, Opcodes, TargetPos, Value) ->
    Offset = TargetPos - CurPos,
    {List1, List2} = lists:split(Offset, Opcodes),
    List1 ++ [Value | tl(List2)].

runcode(Opcodes) ->
    runcode(Opcodes, 0, Opcodes).
runcode([99 | _], _Position, Allcodes) ->
    Allcodes;
runcode([Op, P1, P2, T | Rest], Position, Allcodes) ->
    Term1 = readmem(P1, Allcodes),
    Term2 = readmem(P2, Allcodes),
    Result = compute(Op, Term1, Term2),
    NewPosition = Position + 4,
    NewOpcodes  = replacecode(NewPosition, Rest, T, Result),
    NewAllcodes = replacecode(0, Allcodes, T, Result),
    runcode(NewOpcodes, NewPosition, NewAllcodes).

tryallcodes(Opcodes, TargetCode) ->
    tryallcodes(0, 0, Opcodes, TargetCode).
tryallcodes(Noun, Verb, Opcodes0, TargetCode) ->
    Opcodes1 = replacecode(0, Opcodes0, 1, Noun),
    Opcodes  = replacecode(0, Opcodes1, 2, Verb),
    case runcode(Opcodes) of
	[TargetCode | _] -> 100*Noun+Verb;
	_WrongCode ->
	    {NewNoun, NewVerb} = case {Noun, Verb} of
				     {_, 99} -> {Noun+1, 0};
				     _Else   -> {Noun, Verb+1}
				 end,
	    tryallcodes(NewNoun, NewVerb, Opcodes0, TargetCode)
    end.

go() ->
    {ok, [Opcodes0]} = file:consult("input.txt"),
    Opcodes1 = replacecode(0, Opcodes0, 1, 12),
    Opcodes  = replacecode(0, Opcodes1, 2, 2),
    Answer1 = runcode(Opcodes),
    TargetCode = 19690720,
    Answer2 = tryallcodes(Opcodes0, TargetCode).
