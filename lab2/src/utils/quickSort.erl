%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Mar 2025 17:16
%%%-------------------------------------------------------------------
-module(quickSort).
-author("wiktor").

%% API
-export([qs/1, random_elems/3, compare_speeds/3]).

less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).

random_elems(N,Min,Max)-> [rand:uniform(Max - Min + 1) + Min - 1 || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  {T1, S1} = timer:tc(Fun1, [List]),
  {T2, S2} = timer:tc(Fun2, [List]),
  io:format("Fun1 res: ~p~n", [S1]),
  io:format("Fun2 res: ~p~n", [S2]),
  io:format("Fun1 ~p~n", [T1]),
  io:format("Fun2 ~p~n", [T2]).
