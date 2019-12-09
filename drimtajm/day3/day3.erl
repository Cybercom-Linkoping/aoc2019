-module('day3').
-compile('export_all').

read_wires(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_wire_paths(Device, [])
      after file:close(Device)
    end.

get_wire_paths(Device, Wires) ->
    case file:read_line(Device) of
        eof        ->
	    Wires;
        {ok, Data} ->
	    Wire = get_wire_path(string:strip(Data, right, $\n)),
	    get_wire_paths(Device, [Wire | Wires])
    end.

get_wire_path(String) ->
    Tokens = string:tokens(String, ","),
    {Tokens, get_wire_path(Tokens, 0, 0, 0, [], [])}.

get_wire_path([], _FinalX, _FinalY, _Dist, HorizontalLines, VerticalLines) ->
    [{horizontal, HorizontalLines}, {vertical, VerticalLines}];
get_wire_path([[$R | Right] | RestPath], X, Y, Distance, HLines, VLines) ->
    DeltaX = list_to_integer(Right),
    NewX = X + DeltaX,
    NewDistance = Distance + DeltaX,
    NewHLines = [{{X, NewX}, Y, Distance, right} | HLines],
    get_wire_path(RestPath, NewX, Y, NewDistance, NewHLines, VLines);
get_wire_path([[$L | Left] | RestPath], X, Y, Distance, HLines, VLines) ->
    DeltaX = list_to_integer(Left),
    NewX = X - DeltaX,
    NewDistance = Distance + DeltaX,
    NewHLines = [{{NewX, X}, Y, Distance, left} | HLines],
    get_wire_path(RestPath, NewX, Y, NewDistance, NewHLines, VLines);
get_wire_path([[$U | Up] | RestPath], X, Y, Distance, HLines, VLines) ->
    DeltaY = list_to_integer(Up),
    NewY = Y + DeltaY,
    NewDistance = Distance + DeltaY,
    NewVLines = [{X, {Y, NewY}, Distance, up} | VLines],
    get_wire_path(RestPath, X, NewY, NewDistance, HLines, NewVLines);
get_wire_path([[$D | Down] | RestPath], X, Y, Distance, HLines, VLines) ->
    DeltaY = list_to_integer(Down),
    NewY = Y - DeltaY,
    NewDistance = Distance + DeltaY,
    NewVLines = [{X, {NewY, Y}, Distance, down} | VLines],    
    get_wire_path(RestPath, X, NewY, NewDistance, HLines, NewVLines).

get_all_crossing_points([{horizontal, HLines1}, {vertical, VLines1}],
			[{horizontal, HLines2}, {vertical, VLines2}]) ->
    get_crossing_points(HLines1, VLines2, []) 
	++ get_crossing_points(HLines2, VLines1, []).

get_crossing_points([], _VLines, CPs) ->
    CPs;
get_crossing_points([HLine | RestHLines], VLines, CPs) ->
    get_crossing_points(RestHLines, VLines,
			CPs ++ get_crossing_points_for_line(HLine, VLines)).

get_crossing_points_for_line(HLine, VLines) ->
    get_crossing_points_for_line(HLine, VLines, []).
get_crossing_points_for_line(_HLine, [], CPs) ->
    CPs;
get_crossing_points_for_line({{MinX, MaxX}, Y, D1, R1} = HLine, 
			     [{X, {MinY, MaxY}, D2, R2} | RestVLines], CPs) ->
    if ((MinX =< X) andalso (MaxX >= X) andalso
	(MinY =< Y) andalso (MaxY >= Y)) andalso ({X, Y} =/= {0, 0}) ->
	    get_crossing_points_for_line(HLine, RestVLines, [{X, Y} | CPs]);
       true ->
	    get_crossing_points_for_line(HLine, RestVLines, CPs)
    end.

retrace(WireData, CrossingPoints, CPsWDistances) ->
    CPsWDistances.
combine(CPs, CPsWDs1, CPsWDs2) ->
    [x].

go() ->
    [{WireData1, Wire1}, {WireData2, Wire2}] = read_wires("input.txt"),
    CrossingPoints = get_all_crossing_points(Wire1, Wire2),
    io:format("Crossing points: ~p~nWires: ~p~n", [CrossingPoints, [Wire1, Wire2]]),
    [{horizontal, HLines1}, {vertical, VLines1}] = Wire1,
    [{horizontal, HLines2}, {vertical, VLines2}] = Wire2,
    SameHLines = [{{X1, X2}, {X3, X4}, Y} || {{X1, X2}, Y} <- HLines1,
					     {{X3, X4}, Y0} <- HLines2,
					     Y == Y0],
    SameVLines = [{X, {Y1, Y2}, {Y3, Y4}} || {X, {Y1, Y2}} <- VLines1,
					     {X0, {Y3, Y4}} <- VLines2,
					     X == X0],
    io:format("Same Hlines: ~p~nSame VLines: ~p~n", [SameHLines, SameVLines]),
    Distances = lists:map(fun ({X, Y}) -> abs(X) + abs(Y) end, CrossingPoints),
    io:format("Answer1: ~p~n", [hd(lists:sort(Distances))]),
    CPsWDistances1 = retrace(WireData1, CrossingPoints, []),
    CPsWDistances2 = retrace(WireData2, CrossingPoints, []),
    WalkingDistances = combine(CrossingPoints, CPsWDistances1, CPsWDistances2),
    io:format("Answer2: ~p~n", [hd(lists:sort(WalkingDistances))]).
