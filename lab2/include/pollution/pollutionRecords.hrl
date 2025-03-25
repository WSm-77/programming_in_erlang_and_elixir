%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2025 11:48
%%%-------------------------------------------------------------------
-author("wiktor").

-record(station, {
  name,
  coordinates
}).

-record(measurement, {
  stationCoordinates, %% {x, y}
  datetime, %% {{yyyy, mm, dd}, {hh, mm, ss}}
  type %% string
}).

-record(monitor, {
  %% converts station name or coordinates to station record
  stationToStationRecMap = #{},

  %% converts {coordinates, datetime, type} to measurement value
  measurementToVal = #{}
}).
