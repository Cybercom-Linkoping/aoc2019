-module('day4').
-compile('export_all').

count_pws(X, Count) when X > "864247" ->
    Count;
count_pws(X, Count) ->
    case has_double(X) of
	true  ->
	    io:format("~p ~p~n", [Count+1, X]),  %% for debugging
	    count_pws(get_next(X), Count+1);
	false -> count_pws(get_next(X), Count)
    end.		     

has_double([]) ->                                %% no more digits to try
    false;
has_double([X, X]) ->                            %% two of the same in the end
    true;
has_double([X, X, X0 | _]) when X =/= X0 ->      %% exactly two of the same
    true;
has_double([X, X, X | Rest]) ->     %% remove all "more than 2" of the same
    has_double(lists:dropwhile(fun (X0) -> X0 == X end, Rest)); %% check rest
has_double([_ | Rest]) ->           %% first two digits differ, check from
    has_double(Rest).               %% the second digit

get_next(X) ->
    lists:reverse(increase(lists:reverse(X), 1)).

increase([First, Next | Rest], Count)
  when (((First + 1) rem ($9+1))+$0 < Next) ->
    increase([Next | Rest], Count+1);
increase([First | Rest], Count) ->
    lists:duplicate(Count, First+1) ++ Rest.

go() ->
    %% "Cheating": Starting number is 402328, but since 0<4, 444444 is the
    %% first possible pw (i.e. not decreasing but not increasing either)
    count_pws("444444", 0).
