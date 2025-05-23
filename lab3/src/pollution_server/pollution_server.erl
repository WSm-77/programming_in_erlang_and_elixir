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

%% API
-export([start/0, init/0, init/1]).
-export([add_station/2, get_monitor/0, start/1, stop/0, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2,
  get_station_min/2, get_daily_mean/2, get_maximum_gradient_stations/2]).

start() ->
  ServerPID = spawn(pollution_server, init, []),
  register(server, ServerPID).

start(Monitor) ->
  ServerPID = spawn(pollution_server, init, [Monitor]),
  register(server, ServerPID).

stop() ->
  server ! stop,
  unregister(server).

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
    stop -> ok
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

%% add_value/4 - adds a reading from the station (geographic coordinates or station name, date, measurement type, value);

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

%% remove_value/3 - removes a reading from the station (geographic coordinates or station name, date, measurement type);

remove_value(Station, Datetime, Type) ->
  server ! {get, self()},
  receive
    Monitor -> handle_remove_value(Station, Datetime, Type, Monitor)
  end.

handle_remove_value(Station, Datetime, Type, Monitor) ->
  case pollution:remove_value(Station, Datetime, Type, Monitor) of
    {error, Message} -> {error, Message};
    NewMonitor ->
      server ! {update, NewMonitor},
      NewMonitor
  end.

%% get_one_value/3 - returns the measurement value from the specified station of the specified type and date;

get_one_value(Station, Datetime, Type) ->
  server ! {get, self()},
  receive
    Monitor -> pollution:get_one_value(Station, Datetime, Type, Monitor)
  end.

%% get_station_min/2 - returns the minimum value of a parameter from the specified station and type;

get_station_min(Station, Type) ->
  server ! {get, self()},
  receive
    Monitor -> pollution:get_station_min(Station, Type, Monitor)
  end.

%% get_station_mean/2 - returns the average value of a parameter from the specified station and type;

get_station_mean(Station, Type) ->
  server ! {get, self()},
  receive
    Monitor -> pollution:get_station_mean(Station, Type, Monitor)
  end.

%% get_daily_mean/2 - returns the average value of a specified parameter type on a specified day across all stations;

get_daily_mean(Type, Date) ->
  server ! {get, self()},
  receive
    Monitor -> pollution:get_daily_mean(Type, Date, Monitor)
  end.

%% get_maximum_gradient_stations/2

get_maximum_gradient_stations(Type, Date) ->
  server ! {get, self()},
  receive
    Monitor -> pollution:get_maximum_gradient_stations(Type, Date, Monitor)
  end.
