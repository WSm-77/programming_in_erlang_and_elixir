%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2025 10:53
%%%-------------------------------------------------------------------
-module(pollution_gen_server_test).
-author("wiktor").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([run_all_tests/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_station_test() ->
  M2 = pollution_gen_server:add_station("Stacja 1", {1,1}),
  ?assertNotMatch({error, _}, M2),
  ?assertMatch({error, _}, pollution_gen_server:add_station("Stacja 1", {1,1})),
  ?assertMatch({error, _}, pollution_gen_server:add_station("Stacja 1", {2,2})),
  ?assertMatch({error, _}, pollution_gen_server:add_station("Stacja 2", {1,1})),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertNotMatch({error, _}, pollution_gen_server:add_value("Stacja 1", Time, "PM1", 46.3)),
  ?assertNotMatch({error, _}, pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),

  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 46.3),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),
  ?assertNotMatch({error, _}, pollution_gen_server:get_monitor()),

  timer:sleep(1100),
  Time2 = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution_gen_server:add_value( {1,1}, Time2, "PM10", 46.3)),
  ?assertNotMatch({error, _}, pollution_gen_server:add_value( {1,1}, Time2, "PM1", 46.3)),
  ?assertNotMatch({error, _}, pollution_gen_server:add_value( {1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3)),

  pollution_gen_server:add_value({1,1}, Time2, "PM10", 46.3),
  pollution_gen_server:add_value({1,1}, Time2, "PM1", 46.3),
  pollution_gen_server:add_value({1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3),
  ?assertNotMatch({error, _}, pollution_gen_server:get_monitor()),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_fail_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  ?assertNotMatch({error, _}, pollution_gen_server:get_monitor()),
  ?assertMatch({error, _}, pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_gen_server:add_value("Stacja 1", Time, "PM10", 36.3)),
  ?assertMatch({error, _}, pollution_gen_server:add_value({1,1}, Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_gen_server:add_value({1,1}, Time, "PM10", 36.3)),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_non_existing_station_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_gen_server:add_value("Stacja 2", calendar:local_time(), "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_gen_server:add_value({1,2}, calendar:local_time(), "PM10", 46.3)),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 46.3),
  M3 = pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  M4 = pollution_gen_server:remove_value("Stacja 1", Time, "PM10"),
  ?assertNotMatch({error, _}, M4),
  ?assertNotEqual(M4, M3),
  M5 = pollution_gen_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"),
  ?assertNotMatch({error, _}, M5),
  ?assertNotEqual(M5, M4),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_and_add_back_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 46.3),
  M3 = pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  M4 = pollution_gen_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"),
  ?assertNotEqual(M4, M3),

  M5 = pollution_gen_server:add_value({1,1}, {{2023,3,27},{11,16,9}}, "PM10", 46.3),
  ?assertEqual(M5, M3),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_fail_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 46.3),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  ?assertMatch({error, _}, pollution_gen_server:remove_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:remove_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_gen_server:remove_value({1,2}, Time, "PM10")),
  ?assertMatch({error, _}, pollution_gen_server:remove_value("Stacja 2", Time, "PM10")),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch(46.3, pollution_gen_server:get_one_value("Stacja 1", Time, "PM10")),
  ?assertMatch(36.3, pollution_gen_server:get_one_value("Stacja 1", Time, "PM1")),
  ?assertMatch(46.3, pollution_gen_server:get_one_value({1,1}, Time, "PM10")),
  ?assertMatch(26.3, pollution_gen_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10")),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_gen_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value({1,1}, Time, "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 2", Time, "PM1")),
  ?assertMatch({error, _}, pollution_gen_server:get_one_value({1,2}, Time, "PM10")),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_mean_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch(15.0, pollution_gen_server:get_station_mean("Stacja 1", "PM10")),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),

  ?assertMatch(40/3, pollution_gen_server:get_station_mean("Stacja 1", "PM10")),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,13}}, "PM10", 20),

  ?assertMatch(15.0, pollution_gen_server:get_station_mean({1,1}, "PM10")),

  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_test() ->
  pollution_gen_server:add_station("Stacja 3", {3,3}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 2000),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch(2000, pollution_gen_server:get_station_min("Stacja 1", "PM10")),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 3000),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220),

  ?assertMatch(2000, pollution_gen_server:get_station_min("Stacja 1", "PM10")),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),
  pollution_gen_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234),

  ?assertMatch(10, pollution_gen_server:get_station_min("Stacja 1", "PM10")),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_fail_test() ->
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_gen_server:get_station_min("Stacja 1", "PM10")),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch({error, _}, pollution_gen_server:get_station_min("Stacja 1", "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_station_min("Stacja 3", "PM10")),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_mean_fail_test() ->
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_gen_server:get_station_mean("Stacja 1", "PM10")),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  ?assertMatch({error, _}, pollution_gen_server:get_station_mean("Stacja 1", "PM25")),
  ?assertMatch({error, _}, pollution_gen_server:get_station_mean("Stacja 2", "PM25")),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_test() ->
  pollution_gen_server:add_station("Stacja 3", {3,3}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch(15.0, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220),

  ?assertMatch(15.0, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),
  pollution_gen_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234),

  ?assertMatch(258.8, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_fail_test() ->
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM10",{2023,3,27})),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM25",{2023,3,27})),
  ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM10",{2023,3,29})),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_maximum_gradient_stations_test() ->
  pollution_gen_server:add_station("Stacja 3", {3,3}),
  pollution_gen_server:add_station("Stacja 2", {2,2}),
  pollution_gen_server:add_station("Stacja 1", {1,1}),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 5),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 25),

  ?assertMatch({"Stacja 1","Stacja 2"}, pollution_gen_server:get_maximum_gradient_stations("PM10", {2023,3,27})),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220),
  pollution_gen_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),

  ?assertMatch({"Stacja 1","Stacja 2"}, pollution_gen_server:get_maximum_gradient_stations("PM10", {2023,3,27})),

  pollution_gen_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 50),

  ?assertMatch({"Stacja 2","Stacja 3"}, pollution_gen_server:get_maximum_gradient_stations("PM10", {2023,3,27})),

  pollution_gen_server:add_value("Stacja 1", {{2023,3,27},{11,16,19}}, "PM10", 15),
  pollution_gen_server:add_value("Stacja 2", {{2023,3,27},{11,16,20}}, "PM10", 100),

  ?assertMatch({"Stacja 1","Stacja 2"}, pollution_gen_server:get_maximum_gradient_stations("PM10", {2023,3,27})),
  pollution_gen_server:restart(),
  io:format("Test ~p: OK~n", [?FUNCTION_NAME]),
  ok.

run_all_tests() ->
  add_station_test(),
  add_value_test(),
  add_value_fail_test(),
  add_value_non_existing_station_test(),
  remove_value_test(),
  remove_value_and_add_back_test(),
  remove_value_fail_test(),
  get_one_value_test(),
  get_one_value_fail_test(),
  get_station_mean_test(),
  get_station_min_test(),
  get_station_min_fail_test(),
  get_station_mean_fail_test(),
  get_daily_mean_test(),
  get_daily_mean_fail_test(),
  get_maximum_gradient_stations_test().
