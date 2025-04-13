%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2025 22:08
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("wiktor").

-include_lib("eunit/include/eunit.hrl").
%%-export([run_all_tests/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_station_test() ->
  pollution_server:start(),
  M2 = pollution_server:add_station("Stacja 1", {1,1}),
  ?assertNotMatch({error, _}, M2),
  ?assertMatch({error, _}, pollution_server:add_station("Stacja 1", {1,1})),
  ?assertMatch({error, _}, pollution_server:add_station("Stacja 1", {2,2})),
  ?assertMatch({error, _}, pollution_server:add_station("Stacja 2", {1,1})),
  pollution_server:stop(),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM1", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),

  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),
  ?assertNotMatch({error, _}, pollution_server:get_monitor()),

  timer:sleep(1100),
  Time2 = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, Time2, "PM10", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, Time2, "PM1", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3)),

  pollution_server:add_value({1,1}, Time2, "PM10", 46.3),
  pollution_server:add_value({1,1}, Time2, "PM1", 46.3),
  pollution_server:add_value({1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3),
  ?assertNotMatch({error, _}, pollution_server:get_monitor()),
  pollution_server:stop(),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_fail_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  ?assertNotMatch({error, _}, pollution_server:get_monitor()),
  ?assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 36.3)),
  ?assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 36.3)),
  pollution_server:stop(),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_non_existing_station_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_server:add_value("Stacja 2", calendar:local_time(), "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value({1,2}, calendar:local_time(), "PM10", 46.3)),
  pollution_server:stop(),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  M3 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  M4 = pollution_server:remove_value("Stacja 1", Time, "PM10"),
  ?assertNotMatch({error, _}, M4),
  ?assertNotEqual(M4, M3),
  M5 = pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"),
  ?assertNotMatch({error, _}, M5),
  ?assertNotEqual(M5, M4),
  pollution_server:stop(),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_and_add_back_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  M3 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  M4 = pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"),
  ?assertNotEqual(M4, M3),

  M5 = pollution_server:add_value({1,1}, {{2023,3,27},{11,16,9}}, "PM10", 46.3),
  ?assertEqual(M5, M3),
  pollution_server:stop(),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_fail_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  ?assertMatch({error, _}, pollution_server:remove_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_server:remove_value({1,2}, Time, "PM10")),
  ?assertMatch({error, _}, pollution_server:remove_value("Stacja 2", Time, "PM10")),
  pollution_server:stop(),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch(46.3, pollution_server:get_one_value("Stacja 1", Time, "PM10")),
  ?assertMatch(36.3, pollution_server:get_one_value("Stacja 1", Time, "PM1")),
  ?assertMatch(46.3, pollution_server:get_one_value({1,1}, Time, "PM10")),
  ?assertMatch(26.3, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10")),
  pollution_server:stop(),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_server:get_one_value({1,1}, Time, "PM25")),
  ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 2", Time, "PM1")),
  ?assertMatch({error, _}, pollution_server:get_one_value({1,2}, Time, "PM10")),
  pollution_server:stop(),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_mean_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch(15.0, pollution_server:get_station_mean("Stacja 1", "PM10")),

  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),

  ?assertMatch(40/3, pollution_server:get_station_mean("Stacja 1", "PM10")),

  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,13}}, "PM10", 20),

  ?assertMatch(15.0, pollution_server:get_station_mean({1,1}, "PM10")),

  pollution_server:stop(),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 3", {3,3}),
  pollution_server:add_station("Stacja 2", {2,2}),
  pollution_server:add_station("Stacja 1", {1,1}),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 2000),
  pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch(2000, pollution_server:get_station_min("Stacja 1", "PM10")),

  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 3000),
  pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100),
  pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220),

  ?assertMatch(2000, pollution_server:get_station_min("Stacja 1", "PM10")),

  pollution_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 10),
  pollution_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),
  pollution_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234),

  ?assertMatch(10, pollution_server:get_station_min("Stacja 1", "PM10")),
  pollution_server:stop(),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_fail_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 2", {2,2}),
  pollution_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_server:get_station_min("Stacja 1", "PM10")),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch({error, _}, pollution_server:get_station_min("Stacja 1", "PM25")),
  ?assertMatch({error, _}, pollution_server:get_station_min("Stacja 3", "PM10")),
  pollution_server:stop(),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%get_station_mean_fail_test() ->
%%  M = pollution_server:add_station("Stacja 1", {1,1}, pollution_server:create_monitor()),
%%  ?assertMatch({error, _}, pollution_server:get_station_mean("Stacja 1", "PM10", M)),
%%  M1 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
%%  ?assertMatch({error, _}, pollution_server:get_station_mean("Stacja 1", "PM25", M1)),
%%  ?assertMatch({error, _}, pollution_server:get_station_mean("Stacja 2", "PM25", M1)).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%get_daily_mean_test() ->
%%  M = pollution_server:add_station("Stacja 3", {3,3}, pollution_server:add_station("Stacja 2", {2,2}, pollution_server:add_station("Stacja 1", {1,1}, pollution_server:create_monitor()))),
%%  M1 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
%%  M2 = pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20, M1),
%%  M3 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10, M2),
%%  M4 = pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20, M3),
%%
%%  M5 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100, M4),
%%  M6 = pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220, M5),
%%
%%  M7 = pollution_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000, M6),
%%  M8 = pollution_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000, M7),
%%
%%  M9 = pollution_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234, M8),
%%
%%  ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27}, M2)),
%%  ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27}, M6)),
%%%%  ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27}, M9)).   %% incorrect test case
%%  ?assertMatch(258.8, pollution_server:get_daily_mean("PM10",{2023,3,27}, M9)).
%%
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%get_daily_mean_fail_test() ->
%%  M = pollution_server:add_station("Stacja 2", {2,2}, pollution_server:add_station("Stacja 1", {1,1}, pollution_server:create_monitor())),
%%  ?assertMatch({error, _}, pollution_server:get_daily_mean("PM10",{2023,3,27}, M)),
%%  M1 = pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
%%  M2 = pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20, M1),
%%
%%  ?assertMatch({error, _}, pollution_server:get_daily_mean("PM25",{2023,3,27}, M2)),
%%  ?assertMatch({error, _}, pollution_server:get_daily_mean("PM10",{2023,3,29}, M2)).
%%
%%get_maximum_gradient_stations_test() ->
%%  Type = "PM10",
%%  Date = {2023, 3, 27},
%%  M = pollution_server:add_station("Stacja 3", {3,3}, pollution_server:add_station("Stacja 2", {2,2}, pollution_server:add_station("Stacja 1", {1,1}, pollution_server:create_monitor()))),
%%  M1 = pollution_server:add_value("Stacja 1", {Date,{11,16,10}}, Type, 5, M),
%%  M2 = pollution_server:add_value("Stacja 2", {Date,{11,16,11}}, Type, 20, M1),
%%  M3 = pollution_server:add_value("Stacja 1", {Date,{11,16,12}}, Type, 10, M2),
%%  M4 = pollution_server:add_value("Stacja 2", {Date,{11,16,13}}, Type, 25, M3),
%%
%%  M5 = pollution_server:add_value("Stacja 1", {Date,{11,16,14}}, "PM25", 100, M4),
%%  M6 = pollution_server:add_value("Stacja 2", {Date,{11,16,15}}, "PM25", 220, M5),
%%
%%  M7 = pollution_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, Type, 2000, M6),
%%  M8 = pollution_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, Type, 3000, M7),
%%
%%  M9 = pollution_server:add_value("Stacja 3", {Date,{11,16,18}}, Type, 50, M8),
%%  M10 = pollution_server:add_value("Stacja 1", {Date,{11,16,19}}, Type, 15, M9),
%%  M11 = pollution_server:add_value("Stacja 2", {Date,{11,16,20}}, Type, 100, M10),
%%
%%  ?assertMatch({"Stacja 1","Stacja 2"}, pollution_server:get_maximum_gradient_stations(Type, Date, M4)),
%%  ?assertMatch({"Stacja 1","Stacja 2"}, pollution_server:get_maximum_gradient_stations(Type, Date, M8)),
%%  ?assertMatch({"Stacja 2","Stacja 3"}, pollution_server:get_maximum_gradient_stations(Type, Date, M9)),
%%  ?assertMatch({"Stacja 1","Stacja 2"}, pollution_server:get_maximum_gradient_stations(Type, Date, M11)).

%%run_all_tests() ->
%%  create_monitor_test(),
%%  add_station_test(),
%%  add_value_test(),
%%  add_value_fail_test(),
%%  add_value_non_existing_station_test(),
%%  remove_value_test(),
%%  remove_value_and_add_back_test(),
%%  remove_value_fail_test(),
%%  get_one_value_test(),
%%  get_one_value_fail_test(),
%%  get_station_mean_test(),
%%  get_station_min_test(),
%%  get_station_min_fail_test(),
%%  get_station_mean_fail_test(),
%%  get_daily_mean_test(),
%%  get_daily_mean_fail_test(),
%%  get_maximum_gradient_stations_test().
