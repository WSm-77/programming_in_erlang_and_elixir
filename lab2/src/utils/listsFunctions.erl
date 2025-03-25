%%%-------------------------------------------------------------------
%%% @author wiktor
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Mar 2025 18:00
%%%-------------------------------------------------------------------
-module(listsFunctions).
-author("wiktor").

%% API
-export([l1/0, get_replace_o_a_fun/0, get_list_of_measurements_lists_fun/0, get_flat_list_of_measurements_fun/0, get_filter_measurement_type_fun/0, get_mean/1, calculate_mean/2]).

l1() -> "ala ma kota".

get_replace_o_a_fun() -> fun (Str) ->
      lists:map(
        fun (Chr) ->
          case Chr of
            $o -> $a;
            $a -> $e;
            _ -> Chr
          end
        end,
        Str
      )
  end.

get_list_of_measurements_lists_fun() ->
  fun (List) ->
    lists:map(
      fun ({_, _, _, Mes}) -> Mes end,
      List
    )
  end.

get_flat_list_of_measurements_fun() ->
  fun (List) ->
    lists:foldl(
      fun (MesList, Acc) -> Acc ++ MesList end,
      [],
      List
    )
  end.

get_filter_measurement_type_fun() ->
  fun (List, Type) ->
    lists:foldl(
      fun (Mes, Acc) ->
        case Mes of
          {Type, Val} -> Acc ++ [Val];
          _ -> Acc
        end
      end,
      [],
      List
    )
  end.

get_mean(List) -> lists:sum(List) / length(List).

calculate_mean(Readings, Type) ->
  F1 = get_list_of_measurements_lists_fun(),
  F2 = get_flat_list_of_measurements_fun(),
  F3 = get_filter_measurement_type_fun(),
  L1 = F1(Readings),
  L2 = F2(L1),
  L3 = F3(L2, Type),
  get_mean(L3).
