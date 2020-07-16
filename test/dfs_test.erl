%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Feb 2020 09:10
%%%-------------------------------------------------------------------
-module(dfs_test).
-author("heyoka").
%% API
-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

override_test() ->
   Replacements = [
      {<<"number1">>, 22.78897},
      {<<"number2">>, -3.14},
      {<<"number3">>, -556595895},
      {<<"string1">>, <<"this is my string">>},
      {<<"string2">>, <<"string2">>},
      {<<"text1">>, <<"This is my text !">>},
      {<<"dur1">>, <<"33s">>},
      {<<"dur2">>, <<"12h">>},
      {<<"lambda1">>, <<"lambda: \"answer\" != (42 * 1)">>}

   ],
   {NewDFS, ParseResult} = dfs:parse_file("test/override.dfs", [], Replacements),
   {ok, DFSOut} = file:read_file("test/override_result.dfs"),
   ?assertEqual(binary_to_list(DFSOut), NewDFS),
   ExpectedResult =
      {[{{<<"eval">>,1},
         [{lambda,"Answer /= (42 * 1)",[<<"answer">>],["Answer"]}],
         [{<<"add">>,[{int,-3.14}]},
            {<<"message">>,[{text,<<"This is my text !">>}]},
            {<<"timeouts">>,[{duration,<<"33s">>}]},
            {<<"after">>,[{duration,<<"12h">>}]}]}],
         []},
   ?assertEqual(ExpectedResult, ParseResult).

macro_test() ->
   FileName = "test/macro1.dfs",
   {ok, Data1} = file:read_file("test/ctc_module_condition.dfs"),
   StringData1 = binary_to_list(binary:replace(Data1, <<"\\">>, <<>>, [global])),
   {ok, Data2} = file:read_file("test/publish_macro.dfs"),
   StringData2 = binary_to_list(binary:replace(Data2, <<"\\">>, <<>>, [global])),

   {_, Res} = dfs:parse_file(FileName, [], [],
      [{<<"ctc_module_condition">>, StringData1}, {<<"publish_macro">>, StringData2}]),
   Expected =
      {[{{<<"case">>,2},
         [{lambda,"Data_State_Err == 1 orelse Data_State_Warn == 1",
            [<<"data.State.Err">>,<<"data.State.Warn">>],
            ["Data_State_Err","Data_State_Warn"]},
            {lambda,"Data_State_Auto == 1",
               [<<"data.State.Auto">>],
               ["Data_State_Auto"]}],
         [{<<"values">>,[{string,<<"Err">>},{string,<<"Ok">>}]},
            {<<"as">>,[{string,<<"data.condition">>}]},
            {<<"default">>,[{string,<<"Warn">>}]}]},
         {{<<"s7read">>,3},
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
         {{<<"debug">>,5},[],[]},
         {{<<"where">>,6},
            [{lambda,"Val > 2",[<<"val">>],["Val"]}],
            []},
         {{<<"keep">>,7},[],[{<<"fields">>,[{string,<<"val">>}]}]},
         {{<<"set">>,8},
            [],
            [{<<"fields">>,[{string,<<"id">>}]},
               {<<"field_values">>,[{string,<<"myid222">>}]}]},
         {{<<"delete">>,10},
            [],
            [{<<"fields">>,
               [{string,<<"val">>},{string,<<"some stringy">>}]},
               {<<"max">>,[{int,23}]}]},
         {{<<"default">>,11},
            [],
            [{<<"fields">>,[{string,<<"id">>}]},
               {<<"field_values">>,[{string,<<"myid222">>}]}]}],
         [{{<<"delete">>,10},{<<"set">>,8}},
            {{<<"set">>,8},{<<"keep">>,7}},
            {{<<"keep">>,7},{<<"where">>,6}},
            {{<<"default">>,11},{<<"delete">>,10}},
            {{<<"s7read">>,3},{<<"case">>,2}},
            {{<<"where">>,6},{<<"s7read">>,3}},
            {{<<"debug">>,5},{<<"default">>,11}}]},

   ?assertEqual(Expected, Res).


-endif.


