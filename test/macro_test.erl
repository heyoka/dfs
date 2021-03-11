%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Feb 2021 13:37
%%%-------------------------------------------------------------------
-module(macro_test).
-author("heyoka").

%% API
-export([]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_macro_list_test() ->
  FileName = "test/macro_test.dfs",
  MacroData = test_helper:string_data("test/macro_test_macro.dfs"),
  {_, Res} = dfs:parse_file(FileName, [test_lib], [],
    [{<<"macro_test_macro">>, MacroData}]),

  Expected = {[{{<<"some_node">>,2},
    [],
    [{<<"bindings">>,
      [{string,<<"hui-ta">>},
        {string,<<"hui-ti">>},
        {string,<<"hui-to">>}]},
      {<<"key">>,[{string,<<"my_value">>}]},
      {<<"lambda">>,
        [{lambda,"estr:str_replace(Data_str, <<\"/\">>, <<\".\">>)",
          [<<"data.str">>],
          ["Data_str"]}]}]},
    {{<<"where">>,3},
      [{lambda,"test_lib:defined(Data_val)",
        [<<"data.val">>],
        ["Data_val"]}],
      []}],
    [{{<<"where">>,3},{<<"some_node">>,2}}]}

  ,
  ?assertEqual(Expected, Res).

macro_bridge_test() ->
  FileName = "test/dtpcr_cloud_bridge_condition_m.dfs",
  MacroData = test_helper:string_data("test/macro_cloud_bridge.dfs"),
  {_, Res} = dfs:parse_file(FileName, [test_lib], [],
    [{<<"macro_cloud_bridge">>, MacroData}]),

  Expected = {[{{<<"amqp_consume">>,2},
    [],
    [{<<"host">>,[{string,<<"10.10.1.103">>}]},
      {<<"bindings">>,
        [{string,<<"t.sys.x7823.t_2342.pcr.cond.#">>},
          {string,<<"t.sys.x7823.t_2342some_other string">>}]},
      {<<"user">>,[{string,<<"admin">>}]},
      {<<"pass">>,[{string,<<"admin">>}]},
      {<<"exchange">>,[{string,<<"x_tgw_pcr_grip">>}]},
      {<<"queue">>,[{string,<<"q_pcr_grip">>}]}]},
    {{<<"debug">>,3},[],[]},
    {{<<"amqp_publish">>,4},
      [],
      [{<<"port">>,[{int,5671}]},
        {<<"routing_key_lambda">>,
          [{lambda,"estr:str_replace(Topic, <<\" \">>, <<\"\">>)",
            [<<"topic">>],
            ["Topic"]}]},
        {<<"exchange">>,[{string,<<"xchange_1_fanout">>}]},
        {<<"ssl">>,[]}]}],
    [{{<<"debug">>,3},{<<"amqp_consume">>,2}},
      {{<<"amqp_publish">>,4},{<<"debug">>,3}}]}


  ,
  ?assertEqual(Expected, Res).
-endif.