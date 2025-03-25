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
-export([create_monitor/0, add_station/3, test/0, add_value/5]).

%% create_monitor/0 - tworzy i zwraca nowy monitor zanieczyszczeń;
%% add_station/3 - dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
%% add_value/5 - dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
%% remove_value/4 - usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
%% get_one_value/4 - zwraca wartość pomiaru z zadanej stacji o zadanym typie i z zadanej daty;
%% get_station_min/3 - zwraca minimalną wartość parametru z zadanej stacji i danego typu;
%% get_daily_mean/3 - zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;



create_monitor() -> #monitor{}.

add_station(StationName, {X, Y}, Monitor) when is_record(Monitor, monitor) ->
  Coordinates = {X, Y},
  MonitorMap = Monitor#monitor.stationToStationRecMap,
  case MonitorMap of
    #{StationName := _} -> {error, "Station with given name alredy exists!!!"};
    #{Coordinates := _} -> {error, "Station with given coordinates alredy exists!!!"};
    _ ->
      StationRec = #station{name = StationName, coordinates = Coordinates},
      NewMonitorMap = MonitorMap#{StationName => StationRec, Coordinates => StationRec},
      Monitor#monitor{stationToStationRecMap = NewMonitorMap}
  end;

add_station(_, _, _) -> {error, "Invalid arguments!!! Expected call: add_station(StationName, {X, Y}, Monitor),
 where monitor is record of type 'monitor'"}.

add_measurement(Measurement, Val, MeasurementsMap) when is_record(Measurement, measurement) ->
  case MeasurementsMap of
    #{Measurement := _} -> {error, lists:flatten(io:format("Map alredy contains key = ~p!!!", [Measurement]))};
    _ -> MeasurementsMap#{Measurement => Val}
  end;
add_measurement(_, _, _) -> {error, "add_measurement(): Invalid arguments!!!"}.

%%add_value(Station, Datetime, Type, Val, Monitor) when is_record(Monitor, monitor) ->
add_value(Station, Datetime, Type, Val, Monitor) ->
  MonitorStationsMap = Monitor#monitor.stationToStationRecMap,
  if
    not is_map_key(Station, MonitorStationsMap) ->
      {error, lists:flatten(io_lib:format("Station ~p does not exist!!!", [Station]))};
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
  end.
%%add_value(_, _, _, _, _) -> {error, "add_value(): Invalid arguments!!!"}.
%% remove_value() -> ok.
%% get_one_value() -> ok.
%% get_station_min() -> ok.
%% get_daily_mean() -> ok.

test() -> #monitor{}.
