-module('day1').
-compile('export_all').

read_numbers(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_numbers(Device, [])
      after file:close(Device)
    end.

get_all_numbers(Device, Numbers) ->
    case file:read_line(Device) of
        eof        ->
	    lists:reverse(Numbers);
        {ok, Data} ->
	    Number = list_to_integer(string:strip(Data, right, $\n)),
	    get_all_numbers(Device, [Number | Numbers])
    end.

calculate_fuel(Modules) ->
    calculate_fuel(Modules, []).
calculate_fuel([], FuelValues) ->
    lists:reverse(FuelValues);
calculate_fuel([Mass | Rest], FuelValues) ->
    Fuel = (Mass div 3) - 2,
    if (Fuel > 0) -> calculate_fuel(Rest, [Fuel | FuelValues]);
       true       -> calculate_fuel(Rest, FuelValues)
    end.

calculate_total_fuel(Modules) ->
    calculate_total_fuel(Modules, 0).
calculate_total_fuel([], Fuel) ->
    Fuel;
calculate_total_fuel(Modules, Fuel) ->
    FuelValues = calculate_fuel(Modules),
    calculate_total_fuel(FuelValues, Fuel+lists:sum(FuelValues)).

go() ->
    Numbers = read_numbers("input1.txt"),
    calculate_total_fuel(Numbers).
