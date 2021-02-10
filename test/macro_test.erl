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

simple_macro_test() ->
  FileName = "test/macro_test.dfs",
  MacroData = test_helper:string_data("test/macro_test_macro.dfs"),
  {_, Res} = dfs:parse_file(FileName, [], [],
    [{<<"macro_test_macro">>, MacroData}]),

  Expected = {[{{<<"some_node">>,2},
    [],
    [{<<"bindings">>,
      [{string,<<"ta">>},{string,<<"ti">>},{string,<<"to">>}]},
      {<<"key">>,[{string,<<"my_value">>}]}]}],
    []}

  ,
  ?assertEqual(Expected, Res).

-endif.