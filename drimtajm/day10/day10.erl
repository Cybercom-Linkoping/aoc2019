-module(day10).
-compile('export_all').

-record(asteroiddata, {coordinates,
                       visible_asteroids,
                       distancedata}).
-record(distancedata, {angle,
                       angle_decimal,
                       path,
                       coordinates}).

read_asteroids(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_asteroids(Device, [], 0)
    after file:close(Device)
    end.

get_asteroids(Device, Asteroids0, Y) ->
    case file:read_line(Device) of
        eof        ->
            Asteroids0;
        {ok, Data} ->
            AsteroidString = string:strip(Data, right, $\n),
            MaxX = length(AsteroidString) - 1,
            Asteroids = [{X, Y} || {X, C} <- lists:zip(lists:seq(0, MaxX),
                                                       AsteroidString),
                                   C == $#],
            get_asteroids(Device, Asteroids0 ++ Asteroids, Y + 1)
    end.

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

map_asteroid(Asteroid, Asteroids) ->
    AngleData = get_angles(Asteroid, Asteroids),
    Angles = [Angle || #distancedata{angle = Angle} <- AngleData],
    #asteroiddata{coordinates = Asteroid,
                  visible_asteroids = length(lists:usort(Angles)),
                  distancedata = AngleData}.

get_angle({X, Y}) ->
    GCD = abs(gcd(X, Y)),
    {(X div GCD), (Y div GCD)}.

get_angles({X0, Y0} = _Asteroid, Asteroids) ->
    [#distancedata{angle = get_angle({X-X0, Y-Y0}),
                   angle_decimal = math:pi() - math:atan2(X-X0, Y-Y0),
                   path = {X-X0, Y-Y0}, coordinates = {X, Y}}
     || {X, Y} <- Asteroids, {X, Y} =/= {X0, Y0}].

smaller_angle(Distance1, Distance2) ->
    {Distance1#distancedata.angle_decimal, Distance1#distancedata.path}
        < {Distance2#distancedata.angle_decimal, Distance2#distancedata.path}.

shoot_asteroids([], _Angle, Shots, RemainingAsteroids) ->
    %% Made one full turn, shoot remaining asteroids
    shoot_asteroids(lists:reverse(RemainingAsteroids), no_value, Shots, []);
shoot_asteroids([#distancedata{angle = Angle0} = _Asteroid | Asteroids], Angle1,
               Shots, RemainingAsteroids) when Angle0 =:= Angle1 ->
    %% Asteroid is blocked by previously shot one
    shoot_asteroids(Asteroids, Angle1, Shots, [Asteroids | RemainingAsteroids]);
shoot_asteroids([#distancedata{coordinates = {X, Y}} | _Asteroids], _Angle,
               200, _RemainingAsteroids) ->
    %% 200th asteroid to shoot, print result
    io:format("Answer2: X: ~p, Y: ~p, ~p~n", [X, Y, X*100+Y]),
    ok;
shoot_asteroids([#distancedata{angle = Angle} | Asteroids], _OldAngle,
               Shots, RemainingAsteroids) ->
    %% Asteroid is in "shooting range", increase shot count
    shoot_asteroids(Asteroids, Angle, Shots+1, RemainingAsteroids).

go() ->
    Asteroids = read_asteroids("input.txt"),
    AsteroidMap =
        lists:map(fun (Asteroid) -> map_asteroid(Asteroid, Asteroids) end, 
                  Asteroids),
    BestAsteroid = lists:last(lists:keysort(#asteroiddata.visible_asteroids, 
                                            AsteroidMap)),
    io:format("Answer1: ~p at ~p, ~p~n",
              [BestAsteroid#asteroiddata.visible_asteroids,
               element(1, BestAsteroid#asteroiddata.coordinates),
               element(2, BestAsteroid#asteroiddata.coordinates)]),
    %% Part 2
    SurroundingAsteroids = lists:sort(fun smaller_angle/2,
                                      BestAsteroid#asteroiddata.distancedata),
    shoot_asteroids(SurroundingAsteroids, no_value, 1, []),
    ok.
