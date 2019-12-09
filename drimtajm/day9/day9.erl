-module(day9).
-compile('export_all').

go() ->
    Input = 1,
    go(Input).
go(Input) ->
    {ok, [Opcodes]} = file:consult("input.txt"),
    Args = [{caller, self()}, {outputreceiver, no_value}, {program, Opcodes}],
    {ok, _Pid} = intcode:start_link(machine, Args),
    ok = intcode:send_input(machine, Input),
    receive
        {output, machine, Output} -> Output
    end.
