-module(day7).
-compile('export_all').

permute([]) -> [[]];
permute(L) -> [[X|Y] || X<-L, Y<-permute(L--[X])].

create_daisychain(N, Opcodes) ->
    MachineNames =
        lists:map(fun (X) -> list_to_atom("machine" ++ integer_to_list(X))  end,
                  lists:seq(1, N)),
    NextMachineNames = tl(MachineNames) ++ [hd(MachineNames)],
    lists:map(fun ({MachineId, NextMachineId}) ->
                      Args = [{caller, self()}, {outputreceiver, NextMachineId}, {program, Opcodes}],
                      {ok, _Pid} = intcode:start_link(MachineId, Args),
                      MachineId
              end, lists:zip(MachineNames, NextMachineNames)).

go() ->
    Input = 0,
    go(Input).
go(Input) ->
    {ok, [Opcodes]} = file:consult("input.txt"),

    PhasePermutations1 = permute([4, 3, 2, 1, 0]),
    Args = [{caller, self()}, {outputreceiver, no_value}, {program, Opcodes}],
    Outputs1 = lists:map(fun (Phases) ->
                                 lists:foldl(fun (Phase, CurrentIO) ->
                                                     {ok, _Pid} = intcode:start_link(machine, Args),
                                                     ok = intcode:send_input(machine, Phase),
                                                     ok = intcode:send_input(machine, CurrentIO),
                                                     receive
                                                         {output, machine, Output} -> Output
                                                     end
                                             end, Input, Phases)
                         end, PhasePermutations1),
    io:format("Answer 1: ~p~n", [hd(lists:reverse(lists:sort(Outputs1)))]),

    PhasePermutations2 = permute(lists:seq(5, 9)),
    Outputs2 = lists:map(fun (Phases) ->
                                 Machines = create_daisychain(5, Opcodes),
                                 lists:foreach(fun ({MachineId, Phase}) ->
                                                       ok = intcode:send_input(MachineId, Phase)
                                               end, lists:zip(Machines, Phases)),
                                 ok = intcode:send_input(hd(Machines), Input),
                                 lists:last(lists:map(fun (Machine) ->
                                                              receive
                                                                  {output, Machine, Output} -> Output
                                                              end
                                                      end, Machines))
                         end, PhasePermutations2),
    io:format("Answer 2: ~p~n", [hd(lists:reverse(lists:sort(Outputs2)))]),
    ok.
