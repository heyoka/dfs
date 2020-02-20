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


-endif.


