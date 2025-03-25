%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2025 17:39
%%%-------------------------------------------------------------------
-module(pollutionCalc).
-author("wiktor").

%% API
-export([number_of_readings/2, l1/0, l2/0, l3/0, calculate_max/2, calculate_mean/2]).

%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% info %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%

%% example calls:
%% pollutionCalc:number_of_readings(pollutionCalc:l1(), {12,3,2024}).
%% pollutionCalc:calculate_max(pollutionCalc:l3(), "PM2.5").
%%pollutionCalc:calculate_mean(pollutionCalc:l1(), "PM2.5").

%% calculate_max(Readings, Type) -> float
%% calculate_mean(Readings, Type) -> float

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% solution %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

len([]) -> 0;
len([_ | T]) -> 1 + len(T).

%%number_of_readings([], _) -> 0;
%%number_of_readings([H | T], Date) ->
%%  case H of
%%    {_, Date, _, Rec} -> len(Rec);
%%    _ -> 0
%%  end + number_of_readings(T, Date).

%%%%%%%% Ex 1. %%%%%%%%

number_of_readings([], _) -> 0;
number_of_readings([{_, Date, _, Rec} | T], Date) -> len(Rec) + number_of_readings(T, Date);
number_of_readings([_ | T], Date) -> number_of_readings(T, Date).

%%%%%%%% Ex 2. %%%%%%%%

getMeasurementVal([], _) -> {0, false};
getMeasurementVal([H | T], Type) ->
  case H of
    {Type, Val} -> {Val, true};
    _ -> getMeasurementVal(T, Type)
  end.

%%max(Val1, Val2) when Val1 < Val2 -> Val2;
%%max(Val1, _) -> Val1.

calculate_max([], _) -> 0;
calculate_max([{_, _, _, Measurements} | T], Type) ->
  case getMeasurementVal(Measurements, Type) of
    {Val, true} -> max(Val, calculate_max(T, Type));
    _ -> calculate_max(T, Type)
  end.

%%%%%%%% Ex 3. %%%%%%%%

%% calculate_mean_acc(Measurements, Type, Sum, Cnt) -> float

calculate_mean_acc([], _, _, 0) -> 0;  %% avoid division by '0'
calculate_mean_acc([], _, Sum, Cnt) -> Sum / Cnt;
calculate_mean_acc([{_, _, _, Measurements} | T], Type, Sum, Cnt) ->
  case getMeasurementVal(Measurements, Type) of
    {Val, true} -> calculate_mean_acc(T, Type, Sum + Val, Cnt + 1);
    _  -> calculate_mean_acc(T, Type, Sum, Cnt)
  end.

calculate_mean(Readings, Type) -> calculate_mean_acc(Readings, Type, 0, 0).

p1() -> {
  "Gdynia",
  {12, 3, 2024},
  {12, 0, 0},
  [
    {"PM10", 0.5},
    {"PM2.5", 0.1},
    {"PM1", 0.5},
    {"temp", 20}
  ]
}.

p2() -> {
  "Sopot",
  {13, 4, 2024},
  {13, 5, 0},
  [
    {"PM10", 1.5},
    {"temp", 10}
  ]
}.

p3() -> {
  "Kalisz",
  {12, 3, 2024},
  {12, 0, 0},
  [
    {"PM2.5", 0.2},
    {"PM1", 0.5},
    {"temp", 12}
  ]
}.

p4() -> {
  "Radom",
  {2, 3, 2023},
  {1, 0, 14},
  [
    {"PM10", 4.5},
    {"PM2.5", 0.2},
    {"PM1", 2.5},
    {"temp", 17}
  ]
}.

p5() -> {
  "Kalisz",
  {2, 3, 2023},
  {1, 0, 14},
  [
    {"PM2.5", 1.2},
    {"PM1", 2.5},
    {"temp", 17}
  ]
}.

p6() -> {
  "Radom",
  {2, 3, 2023},
  {12, 0, 14},
  [
    {"PM10", 4.5},
    {"PM1", 2.5},
    {"temp", 17}
  ]
}.

l1() -> [p1(), p2(), p3(), p4(), p5(), p6()].
l2() -> [p1(), p3(), p4()].
l3() -> [p1(), p3(), p6()].
