%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2025 17:44
%%%-------------------------------------------------------------------
-module(lab1).
-author("wiktor").

%% API
-export([hello_world/0, hello_world/1]).

hello_world() -> io:format("hello world~n").
hello_world(Whom) -> io:format("hello ~s~n", [Whom]).
