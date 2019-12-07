-module(day6).
-compile('export_all').

read_orbits(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_orbits(Device, [])
    after file:close(Device)
    end.

get_orbits(Device, DirectOrbits) ->
    case file:read_line(Device) of
        eof        ->
            lists:sort(DirectOrbits);
        {ok, Data} ->
            OrbitString = string:strip(Data, right, $\n),
            [Orbitee, Orbiter] = string:tokens(OrbitString, ")"),
            get_orbits(Device, [{Orbiter, Orbitee} | DirectOrbits])
    end.

count_orbits(Orbiter, Orbits, DirectOrbits, Path) ->
    case lists:keysearch(Orbiter, 1, DirectOrbits) of
        false                       -> {Orbits, lists:reverse(Path)};
        {value, {Orbiter, Orbitee}} -> 
            count_orbits(Orbitee, Orbits+1, DirectOrbits, [Orbiter | Path])
    end.

get_unique_path_length([Node | Path1], [Node | Path2]) ->
    get_unique_path_length(Path1, Path2);
get_unique_path_length(Path1, Path2) ->
    length(Path1) + length(Path2).

shortest_path(Node1, Node2, Orbits) ->
    {value, {Node1, _Orbits1, Path1}} = lists:keysearch(Node1, 1, Orbits),
    {value, {Node2, _Orbits2, Path2}} = lists:keysearch(Node2, 1, Orbits),
    get_unique_path_length(lists:reverse(Path1), lists:reverse(Path2)).
    
go() ->
    DirectOrbits = read_orbits("input.txt"),
    {Orbits, SumOrbits} = 
        lists:mapfoldl(fun ({Orbiter, Orbitee}, Sum) ->
                               {Orbits, Path} = count_orbits(Orbitee, 1, DirectOrbits, []),
                               {{Orbiter, Orbits, Path}, Sum+Orbits}
                       end, 0, DirectOrbits),
    io:format("Answer1: ~p~n", [SumOrbits]),
    io:format("~p~n", [Orbits]),
    io:format("Answer2: ~p~n", [shortest_path("SAN", "YOU", Orbits)]).
