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
   Expected = {[{{<<"where">>,1},
      [{lambda,"Val > 1",[<<"val">>],["Val"]}],
      []},
      {{<<"keep">>,2},[],[{<<"fields">>,[{string,<<"val">>}]}]},
      {{<<"set">>,3},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"mqtt_publish">>,6},
         [],
         [{<<"host">>,[{string,<<"127.0.0.1">>}]}]},
      {{<<"delete">>,7},
         [],
         [{<<"fields">>,
            [{string,<<"val">>},{string,<<"some stringy">>}]},
            {<<"max">>,[{int,99}]}]},
      {{<<"default">>,8},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"delete">>,9},
         [],
         [{<<"fields">>,
            [{string,<<"val">>},{string,<<"somestring">>}]},
            {<<"max">>,[{int,23}]}]},
      {{<<"default">>,10},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]}],
      [{{<<"default">>,10},{<<"delete">>,9}},
         {{<<"mqtt_publish">>,6},{<<"default">>,10}},
         {{<<"delete">>,9},{<<"default">>,8}},
         {{<<"delete">>,7},{<<"set">>,3}},
         {{<<"set">>,3},{<<"keep">>,2}},
         {{<<"keep">>,2},{<<"where">>,1}},
         {{<<"default">>,8},{<<"delete">>,7}}]}
   ,

   ?assertEqual(Expected, Res).


multi_2_test() ->
   FileName = "test/multi_macro2.dfs",

   StringData1 = test_helper:string_data("test/macro1.dfs"),
   StringData2 = test_helper:string_data("test/publish_macro.dfs"),
   StringData3 = test_helper:string_data("test/ctc_module_condition.dfs"),

   {_, Res} = dfs:parse_file(FileName, [], [],
      [{<<"macro1">>, StringData1}, {<<"publish_macro">>, StringData2}, {<<"ctc_module_condition">>, StringData3}]),
   Expected = {[{{<<"delete">>,4},
      [],
      [{<<"fields">>,
         [{string,<<"val">>},{string,<<"some stringy">>}]},
         {<<"max">>,[{int,99}]}]},
      {{<<"default">>,5},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"delete">>,6},
         [],
         [{<<"fields">>,
            [{string,<<"val">>},{string,<<"somestring">>}]},
            {<<"max">>,[{int,23}]}]},
      {{<<"default">>,7},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"case">>,8},
         [{lambda,"Data_State_Err == 1 orelse Data_State_Warn == 1",
            [<<"data.State.Warn">>,<<"data.State.Err">>],
            ["Data_State_Warn","Data_State_Err"]},
            {lambda,"Data_State_Auto == 1",
               [<<"data.State.Auto">>],
               ["Data_State_Auto"]}],
         [{<<"values">>,[{string,<<"Err">>},{string,<<"Ok">>}]},
            {<<"as">>,[{string,<<"data.condition">>}]},
            {<<"default">>,[{string,<<"Warn">>}]}]},
      {{<<"s7read">>,9},
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
      {{<<"debug">>,11},[],[]},
      {{<<"where">>,12},
         [{lambda,"Val > 2",[<<"val">>],["Val"]}],
         []},
      {{<<"keep">>,13},[],[{<<"fields">>,[{string,<<"val">>}]}]},
      {{<<"set">>,14},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]},
      {{<<"delete">>,16},
         [],
         [{<<"fields">>,
            [{string,<<"val">>},{string,<<"some stringy">>}]},
            {<<"max">>,[{int,23}]}]},
      {{<<"default">>,17},
         [],
         [{<<"fields">>,[{string,<<"id">>}]},
            {<<"field_values">>,[{string,<<"myid222">>}]}]}],
      [{{<<"debug">>,11},{<<"default">>,17}},
         {{<<"where">>,12},{<<"s7read">>,9}},
         {{<<"s7read">>,9},{<<"case">>,8}},
         {{<<"default">>,17},{<<"delete">>,16}},
         {{<<"keep">>,13},{<<"where">>,12}},
         {{<<"set">>,14},{<<"keep">>,13}},
         {{<<"delete">>,16},{<<"set">>,14}},
         {{<<"default">>,5},{<<"delete">>,4}},
         {{<<"delete">>,6},{<<"default">>,5}},
         {{<<"case">>,8},{<<"default">>,7}},
         {{<<"default">>,7},{<<"delete">>,6}}]},

   ?assertEqual(Expected, Res).


simple_macro_test() ->
   FileName = "test/multi_macro3.dfs",
   MacroData = test_helper:string_data("test/simple_macro.dfs"),
   {_, Res} = dfs:parse_file(FileName, [], [],
      [{<<"simple_macro">>, MacroData}]),
   Expected = {[{{<<"where">>,1},[],[]},
      {{<<"where">>,3},[],[]},
      {{<<"consume">>,4},[],[{<<"what">>,[{string,43}]}]}],
      [{{<<"consume">>,4},{<<"where">>,1}},
         {{<<"where">>,3},{<<"consume">>,4}}]}
   ,
   ?assertEqual(Expected, Res).

-endif.


