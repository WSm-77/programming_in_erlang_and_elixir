%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2025 17:15
%%%-------------------------------------------------------------------
-module(myLists).
-author("wiktor").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).

contains([], _Val) -> false;
contains([H | _T], Val) when H =:= Val -> true;
contains([_H | T], Val) -> contains(T, Val).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H] ++ duplicateElements(T).

sumFloats([]) -> 0;
sumFloats([H | T]) when is_float(H) -> H + sumFloats(T);
sumFloats(_) -> 0.
