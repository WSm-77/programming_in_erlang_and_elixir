%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2025 09:54
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("wiktor").

-behaviour(gen_server).
-include("pollution/pollutionRecords.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/0, restart/0, crash/0]).
-export([get_monitor/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_min/2, get_station_mean/2, get_daily_mean/2, get_maximum_gradient_stations/2]).

%%-export([add_station/2, get_monitor/0, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2,
%%  get_station_min/2, get_daily_mean/2, get_maximum_gradient_stations/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, pollution:create_monitor(), []).

init(Monitor) ->
  {ok, Monitor}.

restart() ->
  gen_server:cast(?MODULE, restart).

crash() ->
  gen_server:call(?MODULE, crash).

error_checker({error, Message}, Monitor) ->
  {reply, {error, Message}, Monitor};

error_checker(NewMonitor, _Monitor) when is_record(NewMonitor, monitor) ->
  {reply, NewMonitor, NewMonitor};

error_checker(ReturnVal, Monitor) ->
  {reply, ReturnVal, Monitor}.

handle_call(get, _From, Monitor) ->
  {reply, Monitor, Monitor};

handle_call({add_station, StationName, Coord}, _From, Monitor) ->
  error_checker(pollution:add_station(StationName, Coord, Monitor), Monitor);

handle_call({add_value, Station, Datetime, Type, Val}, _From, Monitor) ->
  error_checker(
    pollution:add_value(Station, Datetime, Type, Val, Monitor),
    Monitor
  );

handle_call({remove_value, Station, Datetime, Type}, _From, Monitor) ->
  error_checker(
    pollution:remove_value(Station, Datetime, Type, Monitor),
    Monitor
  );

handle_call({get_one_value, Station, Datetime, Type}, _From, Monitor) ->
  error_checker(
    pollution:get_one_value(Station, Datetime, Type, Monitor),
    Monitor
  );

handle_call({get_station_min, Station, Type}, _From, Monitor) ->
  error_checker(
    pollution:get_station_min(Station, Type, Monitor),
    Monitor
  );

handle_call({get_station_mean, Station, Type}, _From, Monitor) ->
  error_checker(
    pollution:get_station_mean(Station, Type, Monitor),
    Monitor
  );

handle_call({get_daily_mean, Type, Date}, _From, Monitor) ->
  error_checker(
    pollution:get_daily_mean(Type, Date, Monitor),
    Monitor
  );

handle_call({get_maximum_gradient_stations, Type, Date}, _From, Monitor) ->
  error_checker(
    pollution:get_maximum_gradient_stations(Type, Date, Monitor),
    Monitor
  ).

handle_cast(restart, _Monitor) ->
  {noreply, pollution:create_monitor()};

handle_cast(Request, Monitor) ->
  io:format("Cast: Invalid request: ~p~n", [Request]),
  {ok, Monitor}.

get_monitor() ->
  gen_server:call(?MODULE, get).

%% add_station/2 - adds a measurement station entry to the monitor (name and geographic coordinates);

add_station(StationName, Coord) ->
  gen_server:call(?MODULE, {add_station, StationName, Coord}).

%% add_value/4 - adds a reading from the station (geographic coordinates or station name, date, measurement type, value);

add_value(Station, Datetime, Type, Val) ->
  gen_server:call(?MODULE, {add_value, Station, Datetime, Type, Val}).

%% remove_value/3 - removes a reading from the station (geographic coordinates or station name, date, measurement type);

remove_value(Station, Datetime, Type) ->
  gen_server:call(?MODULE, {remove_value, Station, Datetime, Type}).

%% get_one_value/3 - returns the measurement value from the specified station of the specified type and date;

get_one_value(Station, Datetime, Type) ->
  gen_server:call(?MODULE, {get_one_value, Station, Datetime, Type}).

%% get_station_min/2 - returns the minimum value of a parameter from the specified station and type;

get_station_min(Station, Type) ->
  gen_server:call(?MODULE, {get_station_min, Station, Type}).

%% get_station_mean/2 - returns the average value of a parameter from the specified station and type;

get_station_mean(Station, Type) ->
  gen_server:call(?MODULE, {get_station_mean, Station, Type}).

%% get_daily_mean/2 - returns the average value of a specified parameter type on a specified day across all stations;

get_daily_mean(Type, Date) ->
  gen_server:call(?MODULE, {get_daily_mean, Type, Date}).

%% get_maximum_gradient_stations/2

get_maximum_gradient_stations(Type, Date) ->
  gen_server:call(?MODULE, {get_maximum_gradient_stations, Type, Date}).
