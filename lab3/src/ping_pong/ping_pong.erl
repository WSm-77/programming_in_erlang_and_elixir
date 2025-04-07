%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2025 17:08
%%%-------------------------------------------------------------------
-module(ping_pong).
-author("wiktor").

%% API
-export([start/0, stop/0, play/1]).

start() ->
  Ping = spawn(fun () -> ping() end),
  register(ping, Ping),
  Pong = spawn(fun () -> pong() end),
  register(pong, Pong).

stop() ->
  ping ! kill,
  pong ! kill.

play(Count) -> ping ! {hit, Count}.

ping() ->
  receive
    kill -> ok;
    {hit, 0} -> ping();
    {hit, Count} when is_number(Count) ->
      io:format("ping: Count=~p~n", [Count]),
      timer:sleep(500),
      pong ! {hit, Count - 1},
      ping()
  after
    20000 -> ok
  end.

pong() ->
  receive
    kill -> ok;
    {hit, 0} -> pong();
    {hit, Count} when is_number(Count) ->
      io:format("pong: Count=~p~n", [Count]),
      timer:sleep(500),
      ping ! {hit, Count - 1},
      pong()
  after
    20000 -> ok
  end.
