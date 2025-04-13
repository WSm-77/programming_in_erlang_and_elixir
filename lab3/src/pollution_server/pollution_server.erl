%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2025 20:10
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("wiktor").

%%-import(pollution, [create_monitor/0, add_station/3, add_value/5, get_one_value/4, get_station_min/3, get_station_mean/3,
%%        get_daily_mean/3]).

%% API
-export([start/0, init/0, init/1]).
-export([add_station/2, get_monitor/0, start/1, stop/0, add_value/4]).

start() ->
  ServerPID = spawn(pollution_server, init, []),
  register(server, ServerPID).

start(Monitor) ->
  ServerPID = spawn(pollution_server, init, [Monitor]),
  register(server, ServerPID).

stop() ->
  server ! stop.

init() ->
  Monitor = pollution:create_monitor(),
  loop(Monitor).

init(Monitor) ->
  loop(Monitor).

loop(Monitor) ->
  receive
    {update, NewMonitor} ->
      loop(NewMonitor);
    {get, PID} ->
      PID ! Monitor,
      loop(Monitor);
    stop ->
      unregister(server),
      ok
  end.

get_monitor() ->
  server ! {get, self()},
  receive
    Monitor -> Monitor
  end.

%% add_station/2 - adds a measurement station entry to the monitor (name and geographic coordinates);

add_station(StationName, Coord) ->
  server ! {get, self()},
  receive
    Monitor -> handle_add_station(StationName, Coord, Monitor)
  end.

handle_add_station(StationName, Coord, Monitor) ->
  case pollution:add_station(StationName, Coord, Monitor) of
    {error, Message} -> {error, Message};
    NewMonitor ->
      server ! {update, NewMonitor},
      NewMonitor
  end.

%% add_value/5 - adds a reading from the station (geographic coordinates or station name, date, measurement type, value);

add_value(Station, Datetime, Type, Val) ->
  server ! {get, self()},
  receive
    Monitor -> handle_add_value(Station, Datetime, Type, Val, Monitor)
  end.

handle_add_value(Station, Datetime, Type, Val, Monitor) ->
  case pollution:add_value(Station, Datetime, Type, Val, Monitor) of
    {error, Message} -> {error, Message};
    NewMonitor ->
      server ! {update, NewMonitor},
      NewMonitor
  end.

%% remove_value/4 - removes a reading from the station (geographic coordinates or station name, date, measurement type);
%% get_one_value/4 - returns the measurement value from the specified station of the specified type and date;
%% get_station_min/3 - returns the minimum value of a parameter from the specified station and type;
%% get_station_mean/3 - returns the average value of a parameter from the specified station and type;
%% get_daily_mean/3 - returns the average value of a specified parameter type on a specified day across all stations;
