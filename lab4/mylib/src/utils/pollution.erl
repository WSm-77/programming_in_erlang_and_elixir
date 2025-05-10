%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2025 10:51
%%%-------------------------------------------------------------------
-module(pollution).
-author("wiktor").

-include("pollution/pollutionRecords.hrl").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_daily_mean/3, get_station_min/3, get_maximum_gradient_stations/3, get_station_mean/3]).

%% create_monitor/0 - creates and returns a new pollution monitor;
%% add_station/3 - adds a measurement station entry to the monitor (name and geographic coordinates), returns the updated monitor;
%% add_value/5 - adds a reading from the station (geographic coordinates or station name, date, measurement type, value), returns the updated monitor;
%% remove_value/4 - removes a reading from the station (geographic coordinates or station name, date, measurement type), returns the updated monitor;
%% get_one_value/4 - returns the measurement value from the specified station of the specified type and date;
%% get_station_min/3 - returns the minimum value of a parameter from the specified station and type;
%% get_station_mean/3 - returns the average value of a parameter from the specified station and type;
%% get_daily_mean/3 - returns the average value of a specified parameter type on a specified day across all stations;


get_station_coordinates(Station, Monitor) ->
  case maps:get(Station, Monitor#monitor.stationToStationRecMap, error) of
    #station{coordinates = StationCoordinates} -> StationCoordinates;
    error -> {error, lists:flatten(io_lib:format("Station ~p does not exist!!!~n", [Station]))}
  end.

%%%%%%%%%%%%%%%% create_monitor() %%%%%%%%%%%%%%%%

create_monitor() -> #monitor{}.

%%%%%%%%%%%%%%%% add_station() %%%%%%%%%%%%%%%%

%% {X, Y} is used only for guards check purpose
add_station(StationName, {X, Y}, Monitor) when is_record(Monitor, monitor), is_number(X), is_number(Y) ->
  Coordinates = {X, Y},
  MonitorMap = Monitor#monitor.stationToStationRecMap,
  case MonitorMap of
    #{StationName := _} -> {error, "Station with given name alredy exists!!!~n"};
    #{Coordinates := _} -> {error, "Station with given coordinates alredy exists!!!~n"};
    _ ->
      StationRec = #station{name = StationName, coordinates = Coordinates},
      NewMonitorMap = MonitorMap#{StationName => StationRec, Coordinates => StationRec},
      Monitor#monitor{stationToStationRecMap = NewMonitorMap}
  end;

add_station(_, _, _) -> {error, "Invalid arguments!!! Expected call: add_station(StationName, {X, Y}, Monitor),
 where monitor is record of type 'monitor'~n"}.

%%%%%%%%%%%%%%%% add_value() %%%%%%%%%%%%%%%%

%% create new map with added measurement for monitor
add_measurement(Measurement, Val, MeasurementsMap) when is_record(Measurement, measurement) ->
  case MeasurementsMap of
    #{Measurement := _} -> {error, lists:flatten(io_lib:format("Map alredy contains key = ~p!!!~n", [Measurement]))};
    _ -> MeasurementsMap#{Measurement => Val}
  end;
add_measurement(_, _, _) -> {error, "add_measurement(): Invalid arguments!!!~n"}.

%% 1. check if station exists
%% 2. get station coordinates
%% 3. create measurement record
%% 4. try to add new measurement
%%  - error: pass error message
%%  - no error: return new monitor instance with updated measurements map
add_value(Station, Datetime, Type, Val, Monitor) when is_record(Monitor, monitor) ->
  MonitorStationsMap = Monitor#monitor.stationToStationRecMap,
  if
    not is_map_key(Station, MonitorStationsMap) ->
      {error, lists:flatten(io_lib:format("Station ~p does not exist!!!~n", [Station]))};
    true ->
      #station{coordinates = StationCoordinates} = maps:get(Station, MonitorStationsMap),
      Measurement = #measurement{
        stationCoordinates = StationCoordinates,
        datetime = Datetime,
        type = Type
      },
      case add_measurement(Measurement, Val, Monitor#monitor.measurementToVal) of
        {error, Message} -> {error, Message};
        NewMeasurementsMap -> Monitor#monitor{measurementToVal = NewMeasurementsMap}
      end
  end;

add_value(_, _, _, _, _) -> {error, "add_value(): Invalid arguments!!!~n"}.

%%%%%%%%%%%%%%%% remove_value() %%%%%%%%%%%%%%%%

remove_value(Station, Datetime, Type, Monitor) when is_record(Monitor, monitor) ->
  MonitorStationMap = Monitor#monitor.stationToStationRecMap,
  if
    not is_map_key(Station, MonitorStationMap) ->
      {error, lists:flatten(io_lib:format("Station ~p does not exist!!!~n", [Station]))};
    true ->
      #station{coordinates = StationCoordinates} = maps:get(Station, MonitorStationMap),
      MeasurementToRemove = #measurement{
        stationCoordinates = StationCoordinates,
        datetime = Datetime,
        type = Type
      },
      if
        not is_map_key(MeasurementToRemove, Monitor#monitor.measurementToVal) ->
          {error, lists:flatten(io_lib:format("Map doesn't contain key ~p!!!~n", [MeasurementToRemove]))};
        true ->
          Monitor#monitor{
            measurementToVal = maps:remove(MeasurementToRemove, Monitor#monitor.measurementToVal)
          }
      end
  end;
remove_value(_, _, _, _) -> {error, "remove_value(): Invalid arguments!!!~n"}.

%%%%%%%%%%%%%%%% get_one_value() %%%%%%%%%%%%%%%%

%%get_one_value("Stacja 1", Time, "PM10", M3)
get_one_value(Station, Datetime, Type, Monitor) when is_record(Monitor, monitor) ->
  case get_station_coordinates(Station, Monitor) of
    {error, Message} -> {error, Message};
    StationCoordinates ->
      Measurement = #measurement{
        stationCoordinates = StationCoordinates,
        datetime = Datetime,
        type = Type
      },
      MonitorMeasurementsMap = Monitor#monitor.measurementToVal,
      maps:get(
        Measurement,
        MonitorMeasurementsMap,
        {error, lists:flatten(io_lib:format("Map doesn't contain key ~p!!!~n", [Measurement]))}
      )
  end;

get_one_value(_, _, _, _) -> {error, "get_one_value(): Invalid arguments!!!~n"}.

get_station_min(Station, Type, Monitor) when is_record(Monitor, monitor) ->
  case get_station_coordinates(Station, Monitor) of
    {error, Message} -> {error, Message};
    StationCoordinates ->
      Values = maps:fold(
        fun (K, V, Acc) ->
          case K of
            #measurement{stationCoordinates = StationCoordinates, type = Type} ->
              Acc ++ [V];
            _ -> Acc
          end
        end,
        [],
        Monitor#monitor.measurementToVal
      ),
      case length(Values) of
        0 -> {error, lists:flatten(io_lib:format("Station ~p does not have measurements of type ~p!!!~n", [Station, Type]))};
        _ -> lists:min(Values)
      end
  end;
get_station_min(_, _, _) -> {error, "get_station_min(): Inavalid arguments!!!~n"}.

get_station_mean(Station, Type, Monitor) when is_record(Monitor, monitor) ->
  case get_station_coordinates(Station, Monitor) of
    {error, Message} -> {error, Message};
    StationCoordinates ->
      Values = maps:fold(
        fun (K, V, Acc) ->
          case K of
            #measurement{stationCoordinates = StationCoordinates, type = Type} ->
              Acc ++ [V];
            _ -> Acc
          end
        end,
        [],
        Monitor#monitor.measurementToVal
      ),
      case length(Values) of
        0 -> {error, lists:flatten(io_lib:format("Station ~p does not have measurements of type ~p!!!~n", [Station, Type]))};
        _ -> lists:sum(Values) / length(Values)
      end
  end;
get_station_mean(_, _, _) -> {error, "get_station_mean(): Inavalid arguments!!!~n"}.

get_daily_mean(Type, Date, Monitor) when is_record(Monitor, monitor) ->
  Values = maps:fold(
    fun (K, V, Acc) ->
      case K of
        #measurement{type = Type, datetime = {Date, _}} -> Acc ++ [V];
        _ -> Acc
      end
    end,
    [],
    Monitor#monitor.measurementToVal
  ),
  case length(Values) of
    0 -> {error, lists:flatten(io_lib:format("No measurements of type ~p found!!!~n", [Type]))};
    N -> lists:sum(Values) / N
  end;

get_daily_mean(_, _, _) -> {error, "get_daily_mean(): Invalid arguments!!!~n"}.

calc_distance({X1, Y1}, {X2, Y2}) ->
  Dx = X1 - X2,
  Dy = Y1 - Y2,
  math:sqrt(Dx * Dx + Dy * Dy).

get_key_with_highest_value(Map) ->
  maps:fold(
    fun (Key, Value, {MaxK, MaxV}) ->
      if
        MaxK =:= error; MaxV < Value -> {Key, Value};
        true -> {MaxK, MaxV}
      end
    end,
    {error, 0},
    Map
  ).

get_maximum_gradient_stations(Type, Date, Monitor) when is_record(Monitor, monitor) ->
%%  io:format("~p~n", [Monitor]),
  StationsDailyMeasurementsMap = maps:fold(
    fun (Measurement, Value, Acc) ->
      case Measurement of
        #measurement{stationCoordinates = StationCoordinates, datetime = {Date, _}, type = Type} ->
          Acc#{StationCoordinates => maps:get(StationCoordinates, Acc, []) ++ [Value]};
        _ ->
          Acc
      end
    end,
    #{},
    Monitor#monitor.measurementToVal
  ),
  StationsMinMaxMeasurementsMap = maps:map(
    fun (_Key, ValuesList) ->
      {lists:min(ValuesList), lists:max(ValuesList)}
    end,
    StationsDailyMeasurementsMap
  ),
  StationCoordinatesList = maps:fold(
    fun (K, _V, Acc) -> Acc ++ [K] end,
    [],
    StationsDailyMeasurementsMap
  ),
  CoordinatesPairs = [{Coord1, Coord2} || Coord1 <- StationCoordinatesList, Coord2 <- StationCoordinatesList, Coord1 =/= Coord2],
  CoordPairToGradient = lists:foldl(
    fun ({Coord1, Coord2}, Acc) ->
      {Min1, _} = maps:get(Coord1, StationsMinMaxMeasurementsMap),
      {_, Max2} = maps:get(Coord2, StationsMinMaxMeasurementsMap),
      Gradient = abs(Max2 - Min1) / calc_distance(Coord1, Coord2),
      #station{name = Station1} = maps:get(Coord1, Monitor#monitor.stationToStationRecMap),
      #station{name = Station2} = maps:get(Coord2, Monitor#monitor.stationToStationRecMap),
      Acc#{{Station1, Station2} => Gradient}
    end,
    #{},
    CoordinatesPairs
  ),
%%  io:format("~p~n", [CoordPairToGradient]),
  case get_key_with_highest_value(CoordPairToGradient) of
    {error, _} -> {error, "No gradient found!!!~n"};
    {StationsPair, _} -> StationsPair
  end;

get_maximum_gradient_stations(_, _, _) -> {error, "get_maximum_gradient_stations(): Invalid arguments!!!~n"}.
