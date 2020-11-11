%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Feb 2020 09:10
%%%-------------------------------------------------------------------
-module(multi_macro_test).
-author("heyoka").
%% API
-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

multi_1_test() ->
   FileName = "test/multi_macro.dfs",

   MacroData = test_helper:string_data("test/publish_macro.dfs"),

   {_, Res} = dfs:parse_file(FileName, [], [],
      [{<<"publish_macro">>, MacroData}]),
   Expected = {[{{<<"where">>,26},
      [{lambda,"Val > 1",[<<"val">>],["Val"]}],
      []},
      {{<<"keep">>,27},[],[{<<"fields">>,[{string,<<"val">>}]}]},
      {{<<"set">>,28},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"mqtt_publish">>,31},
         [],
         [{<<"host">>,[{string,<<"127.0.0.1">>}]}]},
      {{<<"delete">>,32},
         [],
         [{<<"fields">>,
            [{string,<<"val">>},{string,<<"some stringy">>}]},
            {<<"max">>,[{int,99}]}]},
      {{<<"default">>,33},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"delete">>,34},
         [],
         [{<<"fields">>,
            [{string,<<"val">>},{string,<<"somestring">>}]},
            {<<"max">>,[{int,23}]}]},
      {{<<"default">>,35},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]}],
      [{{<<"default">>,35},{<<"delete">>,34}},
         {{<<"mqtt_publish">>,31},{<<"default">>,35}},
         {{<<"delete">>,34},{<<"default">>,33}},
         {{<<"delete">>,32},{<<"set">>,28}},
         {{<<"set">>,28},{<<"keep">>,27}},
         {{<<"keep">>,27},{<<"where">>,26}},
         {{<<"default">>,33},{<<"delete">>,32}}]}
   ,

   ?assertEqual(Expected, Res).


multi_2_test() ->
   FileName = "test/multi_macro2.dfs",

   StringData1 = test_helper:string_data("test/macro1.dfs"),
   StringData2 = test_helper:string_data("test/publish_macro.dfs"),
   StringData3 = test_helper:string_data("test/ctc_module_condition.dfs"),

   {_, Res} = dfs:parse_file(FileName, [], [],
      [{<<"macro1">>, StringData1}, {<<"publish_macro">>, StringData2}, {<<"ctc_module_condition">>, StringData3}]),
   Expected = {[{{<<"delete">>,39},
      [],
      [{<<"fields">>,
         [{string,<<"val">>},{string,<<"some stringy">>}]},
         {<<"max">>,[{int,99}]}]},
      {{<<"default">>,40},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"delete">>,41},
         [],
         [{<<"fields">>,
            [{string,<<"val">>},{string,<<"somestring">>}]},
            {<<"max">>,[{int,23}]}]},
      {{<<"default">>,42},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"case">>,43},
         [{lambda,"Data_State_Err == 1 orelse Data_State_Warn == 1",
            [<<"data.State.Err">>,<<"data.State.Warn">>],
            ["Data_State_Err","Data_State_Warn"]},
            {lambda,"Data_State_Auto == 1",
               [<<"data.State.Auto">>],
               ["Data_State_Auto"]}],
         [{<<"values">>,[{string,<<"Err">>},{string,<<"Ok">>}]},
            {<<"as">>,[{string,<<"data.condition">>}]},
            {<<"default">>,[{string,<<"Warn">>}]}]},
      {{<<"s7read">>,44},
         [],
         [{<<"vars">>,
            [{string,<<"DB12.DBX0.0">>},
               {string,<<"DB12.DBX0.1">>},
               {string,<<"DB12.DBX0.2">>},
               {string,<<"DB12.DBX0.3">>}]},
            {<<"as">>,
               [{string,<<"data.State.Err">>},
                  {string,<<"data.State.Warn">>},
                  {string,<<"data.State.Auto">>},
                  {string,<<"data.State.AutoRdy">>}]}]},
      {{<<"debug">>,46},[],[]},
      {{<<"where">>,47},
         [{lambda,"Val > 2",[<<"val">>],["Val"]}],
         []},
      {{<<"keep">>,48},[],[{<<"fields">>,[{string,<<"val">>}]}]},
      {{<<"set">>,49},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"delete">>,51},
         [],
         [{<<"fields">>,
            [{string,<<"val">>},{string,<<"some stringy">>}]},
            {<<"max">>,[{int,23}]}]},
      {{<<"default">>,52},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]}],
      [{{<<"debug">>,46},{<<"default">>,52}},
         {{<<"where">>,47},{<<"s7read">>,44}},
         {{<<"s7read">>,44},{<<"case">>,43}},
         {{<<"default">>,52},{<<"delete">>,51}},
         {{<<"keep">>,48},{<<"where">>,47}},
         {{<<"set">>,49},{<<"keep">>,48}},
         {{<<"delete">>,51},{<<"set">>,49}},
         {{<<"default">>,40},{<<"delete">>,39}},
         {{<<"delete">>,41},{<<"default">>,40}},
         {{<<"case">>,43},{<<"default">>,42}},
         {{<<"default">>,42},{<<"delete">>,41}}]}
   ,

   ?assertEqual(Expected, Res).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simple_macro_test() ->
   FileName = "test/multi_macro3.dfs",
   MacroData = test_helper:string_data("test/simple_macro.dfs"),
   {_, Res} = dfs:parse_file(FileName, [], [],
      [{<<"simple_macro">>, MacroData}]),
   Expected = {[{{<<"where">>,53},[],[]},
      {{<<"where">>,55},[],[]},
      {{<<"consume">>,56},[],[{<<"what">>,[{string,43}]}]}],
      [{{<<"consume">>,56},{<<"where">>,53}},
         {{<<"where">>,55},{<<"consume">>,56}}]}
   ,
   ?assertEqual(Expected, Res).




-endif.


