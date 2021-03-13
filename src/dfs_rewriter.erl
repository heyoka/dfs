%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2020 08:55
%%%-------------------------------------------------------------------
-module(dfs_rewriter).
-author("heyoka").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(BASE_PATTERN, <<"(?<!%|%%|%\\s)(def\\s*(">>).
-define(PATTERN_NUMBER(VarName),
   [?BASE_PATTERN, VarName, <<")\\s*=[\\s\\n\\t\\r]*-?\\d+\\.?\\d*)(?=\\s|\\n|\\t)">>]).
-define(PATTERN_STRING(VarName),
   [?BASE_PATTERN, VarName, <<")\\s*=[\\t\\r\\s\\n]*('[\\n\\r\\t\\s\\S]*'))">>]).
-define(PATTERN_BOOL(VarName),
   [?BASE_PATTERN, VarName, <<")\\s*=[\\t\\r\\s\\n]*('[\\n\\r\\t\\s\\S]*'))">>]).
-define(PATTERN_TEXT(VarName),
   [?BASE_PATTERN, VarName, <<")\\s*=[\\t\\r\\s\\n]*(<{3}[\\n\\r\\t\\s\\S]*>{3}))">>]).
-define(PATTERN_DURATION(VarName),
   [?BASE_PATTERN, VarName, <<")\\s*=[\\s\\n\\t\\r]*-?\\d+)(w|d|s|m|ms|h)(?=\\s|\\n|\\t|\\z)">>]).
-define(PATTERN_LAMBDA(VarName),
   [?BASE_PATTERN, VarName, <<")\\s*=[\\s\\n\\t\\r]*lambda:\\s+.*)(?=\\R|\\z)">>]).
-define(PATTERN_INLINE(VarName),
   [?BASE_PATTERN, VarName, <<")\\s*=[\\s\\n\\t\\r]*e:\\s+.*)(?=\\R|\\z)">>]).

%% API
-export([execute/3, do/0]).

do() ->
   re:replace(
      "% def fun = lambda: avg(\"vallist\") *2\ndef function = lambda: avg(\"vallist\") * 2\n\n",
      [<<"(?<!%|%%|%\\s)(def\\s*(">>,<<"function">>,<<")\\s*=[\\s\\n\\t\\r]*lambda:.*)(?=\\R)">>] ,
      <<"def function = lambda: string(\"rate\" * 9)">>, [{return, list}, global, multiline, unicode, ungreedy]).


execute([], _, DfsScript) ->
   DfsScript;
execute([{VarName, NewValue}|Replacements], VarList, DfsScript)
   when is_list(VarList), is_list(DfsScript) ->

   NewDfs = replace(VarName, NewValue, VarList, DfsScript),
   execute(Replacements, VarList, NewDfs).

replace(VarName, NewValue, VarList, DFS) ->
   case pattern(VarName, VarList) of
      false -> DFS;
      {Type, Pattern} ->
         DfsValue = to_dfs_var(Type, dfs_std_lib:string(NewValue)),
         Replacement =
            iolist_to_binary([<<"def ">>, VarName, <<" = ">>, DfsValue]),
            re:replace(DFS, Pattern, Replacement,
               [{return, list}, global, multiline, unicode, ungreedy])

   end.

pattern(VarName, VarList) ->
   case proplists:get_value(VarName, VarList) of
      {Type, _Line, _Value} -> {Type, pat(Type, VarName)};
      {lambda,{_Value, _DefsDFS, _DefsErl}} -> {lambda, pat(lambda, VarName)};
      {lambda,_Value, _DefsDFS, _DefsErl} -> {lambda, pat(lambda, VarName)};
      {inline,{_Value, _DefsDFS, _DefsErl}} -> {inline, pat(inline, VarName)};
      {inline,_Value, _DefsDFS, _DefsErl} -> {inline, pat(inline, VarName)};
      _ ->
         %io:format("~nno varlist entry for ~p~n", [VarName]),
         false
   end.

pat(duration, VarName) -> ?PATTERN_DURATION(VarName);
pat(int, VarName) -> ?PATTERN_NUMBER(VarName);
pat(float, VarName) -> ?PATTERN_NUMBER(VarName);
pat(text, VarName) -> ?PATTERN_TEXT(VarName);
pat(string, VarName) -> ?PATTERN_STRING(VarName);
pat(bool, VarName) -> ?PATTERN_BOOL(VarName);
pat(lambda, VarName) -> ?PATTERN_LAMBDA(VarName);
pat(inline, VarName) -> ?PATTERN_INLINE(VarName);
pat(_T, _N) ->
   io:format("~n******************~nno pattern found for type: ~p~n",[{_T, _N}]),
   false.

to_dfs_var(text, Val) -> <<"<<<", Val/binary, ">>>">>;
to_dfs_var(string, Val) -> <<"'", Val/binary, "'">>;
to_dfs_var(_, Val) -> Val.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
re_test() ->
   DFSIn = "
   def hans =
      'wurst'

      def other = <<< {{ignore}} >>>

   def in = 33

   def sql =
      <<<
         SELECT * FROM table
            WHERE bal = 'blu' ORDER BY foo
         def duration
         LIMIT 100;
      >>>

         def duration = 320ms

   |tail(22s)
   .param(22d)

   %def fun = lambda: 44 * max(\"val1\", \"val2\")
   % def fun = lambda: 44 * max(\"val1\", \"val2\")
   def fun = lambda: 44 * max(\"val1\", \"val2\")

   ",
   DFSOut = "
   def hans = 'albers ;)'

      def other = <<<why should we ignore such a beautiful text ?>>>

   def in = 33

   def sql = <<<SELECT other from other table>>>

         def duration = 33s

   |tail(22s)
   .param(22d)

   %def fun = lambda: 44 * max(\"val1\", \"val2\")
   % def fun = lambda: 44 * max(\"val1\", \"val2\")
   def fun = lambda: string(\"rate\" * 9)

   ",
   Out = execute(
      [
         {<<"sql">>, <<"SELECT other from other table">>},
         {<<"duration">>, <<"33s">>},
         {<<"hans">>, <<"albers ;)">>},
         {<<"other">>, <<"why should we ignore such a beautiful text ?">>},
         {<<"fun">>, <<"lambda: string(\"rate\" * 9)">>}
      ],
      [
         {<<"sql">>, {text, 3, <<"value">>}},
         {<<"duration">>, {duration, 4, <<"value">>}},
         {<<"hans">>, {string, 7, <<"value">>}},
         {<<"other">>, {text, 12, <<"value">>}},
         {<<"fun">>, {lambda, 9, <<"value">>}}
      ],
      DFSIn),
   ?assertEqual(DFSOut, Out).


re_2_test() ->
   DFSIn = "
   % def fun = lambda: avg(\"vallist\") *2
   def fun = lambda: avg(\"vallist\") * 2",
   DFSOut = "
   % def fun = lambda: avg(\"vallist\") *2
   def fun = lambda: string(\"rate\" * 9)", %% end of string
   Out = execute(
      [
         {<<"fun">>, <<"lambda: string(\"rate\" * 9)">>}
      ],
      [
         {<<"fun">>,
            {lambda,"dfs_std_lib:string(Rate * 9)",[<<"rate">>],["Rate"]}}
      ],
      DFSIn),
   ?assertEqual(DFSOut, Out).

-endif.