%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Feb 2020 08:55
%%%-------------------------------------------------------------------
-module(dfs_rewrite).
-author("heyoka").

%% API
-export([test_rewrite/0, test/0, execute/3]).

execute([], _, DfsScript) ->
   DfsScript;
execute(Replacements, TabList, DfsScript)
   when is_list(Replacements), is_list(TabList), is_binary(DfsScript) ->
   <<>>.


test() ->
   String = <<"
   def hans =
      'wurst'

   def in = 33
   |tail(22s)
   ">>,
   %Pattern = "[^\r\t\n\s]*def\s*hans\s*=[\s\n]*.+[^\s\n]",
   Pattern = "[\r\t\n\s]*def\s*hans\s*=[\s\n]*.+[^\s\n]",
%%   Pattern = [<<"def">>,<<"\ndef">>,<<"=">>],
   %Pattern = <<"(def)\s*(hans)\s*(=)[\s\n]*(.+)">>,
   Split = [<<"def">>,<<"|">>],
   binary:split(String, Split, [global]).
%%   re:replace(String, Pattern, "def hans = 'albers'",
%%      [{return, list}, anchored, notempty]).

test_rewrite() ->
   String = "
   def hans =
      'wurst'

   def int = 55
   def other = <<< ignore >>>
   ",
   rewriteDFS(String, [
      {<<"hans">>, <<"albers">>},
      {<<"int">>, 22}]).
rewriteDFS(DFSDef, []) -> DFSDef;
rewriteDFS(DFSDef, [{VarName, VarValue}|Vars]) ->

%%   last
%%   [^\r\t\n\s]*(def\s*(other)\s*=[\s\n]*(<{3}[\n\r\t\s\S]*>{3}))


%%% one line text : def text = <<< my text >>> ungreedy
%% **   [^\r\t\n\s]*(def\s*(other)\s*=[\s\n]*(<<<[\n\r\t\s\S]*>>>))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% one line number def int = 456.32 UNgreedy
%%   [^\r\t\n\s]*(def\s*(int)\s*=[\s\n]*[0-9\.]*)(?=\n)

%%   two line number UNgreedy + works for oneliners too
%% **  [^\r\t\n\s]*(def\s*(int)\s*=[\s\n\t\r]*\d+\.?\d*)(?=\s|\n|\t)

%%   one an multiline text UNgreedy :
%%  ** [^\r\t\n\s]*(def\s*(other)\s*=[\t\r\s\n]*(<{3}[\n\r\t\s\S]*>{3}))

%% one and multiline string UNgreedy
%%  ** [^\r\t\n\s]*(def\s*(hans)\s*=[\t\r\s\n]*('[\n\r\t\s\S]*'))

   %% one and multiline duration UNgreedy
%% **  [^\r\t\n\s]*(def\s*(duration)\s*=[\s\n\t\r]*\d+)(w|d|s|m|ms|h)(?=\s|\n|\t)

%%   total
%%   Ss="
%%   [^\r\t\n\s]*(def\s*(duration)\s*=[\s\n\t\r]*[((\d+)(w|d|s|m|ms|h)
%%   (?=\s|\n|\t))|('[\n\r\t\s\S]*')|(<{3}[\n\r\t\s\S]*>{3})|((\d+\.?\d*)(?=\s|\n|\t))]",

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   ^\r\t\n\s]*((def)\s*(hans)\s*=\s*(<{3}[\n\r\t\s\S]*>{3}|('[\n\r\t\s\S]*')|([0-9\.\s\n]*)))
%%   [^\r\t\n\s]*( (def)\s*(int)\s*=((<{3}[\t\r\s\S\n]*>{3})|('[\s\S]*')|[0-9\.\s\n]) )
%%   [^\r\t\n\s]*( (def)\s*(int)\s*=([0-9\.\s\n]*) )
%%
%%
%% [^\r\t\n\s]*((def)\s*(int)\s*=[\t\r\s\S\n<{3}>{3}'")]*)(?=def)
%%   P = <<"[\r\t\n\s]*((def)\s*(hans)\s*=[\s\n]*(.+))[\r\t\n\s]*">>,
   Pattern = iolist_to_binary([<<"[\r\t\n\s]*def\s*">>,VarName,<<"\s*=[\s\n]*(.)+[^\r\t\n\s]">>]),
%%   Pattern = iolist_to_binary([<<"def ">>,VarName,<<" = .+">>]),
   Replacement = iolist_to_binary([<<"def ">>, VarName, <<" = ">>, to_dfs_var(VarValue)]),
   Dfs = re:replace(DFSDef, Pattern, Replacement,[
      {return, binary},
      global, multiline, unicode, ungreedy]),
   io:format("~nDFS after ~p: ~p~n",[{VarName, VarValue}, Dfs]),
   rewriteDFS(Dfs, Vars).

to_dfs_var(Number) when is_number(Number) -> Number;
to_dfs_var(String) when is_binary(String) ->
   <<"<<<", String/binary, ">>>">>;
to_dfs_var(Any) -> Any.
