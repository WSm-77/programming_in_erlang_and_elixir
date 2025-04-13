%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2025 17:46
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("wiktor").

%% API
-export([generate_coordinates/1, dist/2, find_for_person/2, find_closest/2, find_closest_parallel/2]).

generate_coordinates(Count) ->
  Min = 0,
  Max = 10000,
  [{rand:uniform(Max - Min + 1) + Min - 1, rand:uniform(Max - Min + 1) + Min - 1} || _ <- lists:seq(1, Count)].

dist({X1, Y1}, {X2, Y2}) ->
  DX = X1 - X2,
  DY = Y1 - Y2,
  math:sqrt(DX * DX + DY * DY).

find_for_person(PersonLocation, SensorsLocations) ->
  lists:min([{dist(PersonLocation, SensorLocation), {PersonLocation, SensorLocation}} || SensorLocation <- SensorsLocations]).

find_closest(PeopleLocations, SensorsLocations) ->
  Distances = [find_for_person(PersonLocation, SensorsLocations) || PersonLocation <- PeopleLocations],
  {_, Res} = lists:foldl(
    fun ({Dist, PersSens}, Acc) ->
      case Acc of
        {none, none} -> {Dist, PersSens};
        {AccDist, _} when Dist < AccDist -> {Dist, PersSens};
        _ -> Acc
      end
    end,
    {none, none},
    Distances
  ),
  Res.

find_for_person(PersonLocation, SensorsLocations, ParentPID) ->
  Res = lists:min([{dist(PersonLocation, SensorLocation), {PersonLocation, SensorLocation}} || SensorLocation <- SensorsLocations]),
  ParentPID ! Res.

%%find_closest_parallel(PeopleLocations, SensorsLocations) ->
%%  ParentPid = self(),
%%  [spawn(fun () -> find_for_person(PersonLocation, SensorsLocations, ParentPid) end) || PersonLocation <- PeopleLocations],
%%  %% Collect message len(PeopleLocations) times
%%  Distances = [ClosestToPerson || _ <- PeopleLocations, ClosestToPerson <- [receive Msg -> Msg after 5000 -> none end]],
%%  {_, Res} = lists:foldl(
%%    fun ({Dist, PersSens}, Acc) ->
%%      case Acc of
%%        {none, none} -> {Dist, PersSens};
%%        {AccDist, _} when Dist < AccDist -> {Dist, PersSens};
%%        _ -> Acc
%%      end
%%    end,
%%    {none, none},
%%    Distances
%%  ),
%%  Res.

find_closest_parallel(PeopleLocations, SensorsLocations) ->
  ParentPid = self(),
  [spawn(fun () -> find_for_person(PersonLocation, SensorsLocations, ParentPid) end) || PersonLocation <- PeopleLocations],
  %% Collect message len(PeopleLocations) times
  Distances = collect_messages(length(PeopleLocations), []),
  {_, Res} = lists:foldl(
    fun ({Dist, PersSens}, Acc) ->
      case Acc of
        {none, none} -> {Dist, PersSens};
        {AccDist, _} when Dist < AccDist -> {Dist, PersSens};
        _ -> Acc
      end
    end,
    {none, none},
    Distances
  ),
  Res.

collect_messages(0, Acc) ->
  Acc;
collect_messages(N, Acc) ->
  receive
    Msg -> collect_messages(N - 1, [Msg | Acc])
  after 5000 -> %% Timeout after 5000ms
    collect_messages(N - 1, Acc)
  end.
