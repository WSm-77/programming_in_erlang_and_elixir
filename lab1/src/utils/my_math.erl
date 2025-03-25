%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2025 18:05
%%%-------------------------------------------------------------------
-module(my_math).
-author("wiktor").

%% API
-export([power/2]).

%% 1. approach
%% power(_, 0) -> 1;
%% power(Base, Exp) -> Base * power(Base, Exp - 1).

%% 2. approach
power(Base, Exp) ->
  if
    Exp =:= 0 -> 1;
    Exp < 0 -> power(Base, Exp + 1) / Base;
    true -> Base * power(Base, Exp - 1)
  end.
