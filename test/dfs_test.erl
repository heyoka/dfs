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
      %% @todo find out, why bool cannot be replaced
%%      {<<"bool1">>, true},
      {<<"text1">>, <<"This is my text !">>},
      {<<"dur1">>, <<"33s">>},
      {<<"dur2">>, <<"12h">>},
      {<<"lambda1">>, <<"lambda: \"answer\" != (42 * 1)">>}
      %% inline cannot be replaced at the moment
%%      ,
%%      {<<"inline1">>, <<"e: 42 * round(pi())">>}

   ],
   {NewDFS, ParseResult} = dfs:parse_file("test/override.dfs", [], Replacements),
   {ok, DFSOut} = file:read_file("test/override_result.dfs"),
   ?assertEqual(binary_to_list(DFSOut), NewDFS),
   ExpectedResult =
      {[{{<<"eval">>,1},
         [{lambda,"Answer /= (42 * 1)",[<<"answer">>],["Answer"]}],
         [{<<"add">>,[{int,-3.14}]},
            {<<"message">>,[{string,<<"This is my text !">>}]},
            {<<"timeouts">>,[{duration,<<"33s">>}]},
            {<<"after">>,[{duration,<<"12h">>}]},
            {<<"bool">>,[{bool,false}]},
            {<<"inline">>,[{int,168161861}]}]}],
         []}
   ,
   ?assertEqual(ExpectedResult, ParseResult).

macro_test() ->
   FileName = "test/macro1.dfs",
   StringData1 = test_helper:string_data("test/ctc_module_condition.dfs"),
   StringData2 = test_helper:string_data("test/publish_macro.dfs"),

   {_, Res} = dfs:parse_file(FileName, [], [],
      [{<<"ctc_module_condition">>, StringData1}, {<<"publish_macro">>, StringData2}]),
   Expected =
      {[{{<<"case">>,1},
         [{lambda,"Data_State_Err == 1 orelse Data_State_Warn == 1",
            [<<"data.State.Err">>,<<"data.State.Warn">>],
            ["Data_State_Err","Data_State_Warn"]},
            {lambda,"Data_State_Auto == 1",
               [<<"data.State.Auto">>],
               ["Data_State_Auto"]}],
         [{<<"values">>,[{string,<<"Err">>},{string,<<"Ok">>}]},
            {<<"as">>,[{string,<<"data.condition">>}]},
            {<<"default">>,[{string,<<"Warn">>}]}]},
         {{<<"s7read">>,2},
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
         {{<<"debug">>,4},[],[]},
         {{<<"where">>,5},
            [{lambda,"Val > 2",[<<"val">>],["Val"]}],
            []},
         {{<<"keep">>,6},[],[{<<"fields">>,[{string,<<"val">>}]}]},
         {{<<"set">>,7},
            [],
            [{<<"fields">>,[{string,<<"id">>}]},
               {<<"field_values">>,[{string,<<"myid222">>}]}]},
         {{<<"delete">>,9},
            [],
            [{<<"fields">>,
               [{string,<<"val">>},{string,<<"some stringy">>}]},
               {<<"max">>,[{int,23}]}]},
         {{<<"default">>,10},
            [],
            [{<<"fields">>,[{string,<<"id">>}]},
               {<<"field_values">>,[{string,<<"myid222">>}]}]}],
         [{{<<"delete">>,9},{<<"set">>,7}},
            {{<<"set">>,7},{<<"keep">>,6}},
            {{<<"keep">>,6},{<<"where">>,5}},
            {{<<"default">>,10},{<<"delete">>,9}},
            {{<<"s7read">>,2},{<<"case">>,1}},
            {{<<"where">>,5},{<<"s7read">>,2}},
            {{<<"debug">>,4},{<<"default">>,10}}]}
   ,

   ?assertEqual(Expected, Res).

inline_expression_test() ->
   {NewDFS, ParseResult} = dfs:parse_file("test/inline_expr.dfs", [], []),
   {ok, DFSOut} = file:read_file("test/inline_expr_result.dfs"),
   ?assertEqual(binary_to_list(DFSOut), NewDFS),
   ExpectedResult =
      {[{{<<"use_id">>,1},
         [],
         [{<<"use_inline">>,
            [{string,<<"9.73205080756887674909e+00">>}]},
            {<<"use_inline_string">>,[{string,<<"thas as my strang">>}]},
            {<<"inline_expression">>,[{int,27}]},
            {<<"inline_expression_string">>,
               [{string,<<"GESTATTEN: HANS WURSCHT">>}]},
            {<<"lambda">>,
               [{lambda,"5 * 4 + erlang:round(1.21515100000000000335e+01)",
                  [],[]}]},
            {<<"lambda_string">>,
               [{lambda,"estr:str_upcase(<<\"Gestatten: hans wurscht\">>)",
                  [],[]}]}]}],
         []},
?assertEqual(ExpectedResult, ParseResult).

inline_expression_use_ref_test() ->

   ?assertThrow("Reference(s) used in inline-expression: data.separator, data.string_value",
      dfs:parse_file("test/inline_expr_ref.dfs", [], [])).

inline_expression_type_test() ->
   {NewDFS, ParseResult} = dfs:parse_file("test/inline_expr_type_test.dfs", [], []),
   {ok, DFSOut} = file:read_file("test/inline_expr_type_test.dfs"),
   ?assertEqual(binary_to_list(DFSOut), NewDFS),
   ExpectedResult =
      {[{{<<"use">>,1},
         [],
         [{<<"int">>,[{int,11}]},
            {<<"float">>,[{float,9.42477796076938}]},
            {<<"string">>,[{string,<<"Ast.walk">>}]},
            {<<"bool">>,[{bool,true}]},
            {<<"duration">>,[{duration,<<"300ms">>}]},
            {<<"not_duration">>,[{string,<<"500p">>}]},
            {<<"all">>,
               [{int,11},
                  {float,9.42477796076938},
                  {string,<<"Ast.walk">>},
                  {bool,true},
                  {duration,<<"300ms">>},
                  {string,<<"500p">>}]}]}],
         []},
   ?assertEqual(ExpectedResult, ParseResult).


text_new_test() ->
   {NewDFS, ParseResult} = dfs:parse_file("test/text_new.dfs", [test_lib], []),
   {ok, DFSOut} = file:read_file("test/text_new.dfs"),
   ?assertEqual(binary_to_list(DFSOut), NewDFS),
   ExpectedResult =
      {[{{<<"amqp_consume">>,1},
         [],
         [{<<"hop">>,[{string,<<"\"_1202.0014\"">>}]},
            {<<"hap">>,[{string,<<"\"1202.0014\"\"1202_0014\"">>}]},
            {<<"sql">>,
               [{string,<<"\n    SELECT *\n    FROM mytable\n    WHERE\n    $__timefilter\n    AND a > 33\n    AND data['obj1']['3442-5542'] < 2232\n    ">>}]},
            {<<"bindings">>,
               [{string,<<"tgw.data.0x000a.1202.0014.TESTSIM.#">>}]},
            {<<"exchange">>,[{string,<<"x_1202.0014">>}]},
            {<<"queue">>,[{string,<<"q_1202.0014">>}]},
            {<<"topic_as">>,[{string,<<"stream_id">>}]}]},
         {{<<"macro_crate_firehose_batch">>,2},
            [],
            [{<<"site_id">>,[{string,<<"\"0x000a\"">>}]},
               {<<"data_format">>,[{string,<<"\"1202_0014\"">>}]},
               {<<"batch_size">>,[{int,70}]},
               {<<"batch_timeframe">>,[{duration,<<"5s">>}]}]}],
         [{{<<"macro_crate_firehose_batch">>,2},
            {<<"amqp_consume">>,1}}]}

   ,

?assertEqual(ExpectedResult, ParseResult).


-endif.


