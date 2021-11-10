%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Mar 2021 13:15
%%%-------------------------------------------------------------------
-module(test_lib).
-author("heyoka").

-compile(nowarn_export_all).
-compile(export_all).
%% API

defined(Val) ->
  Val /= undefined.
undefined(Val) ->
  Val == undefined.

%%%
str_concat(String1, String2) ->
  unicode:characters_to_binary([String1, String2]).
str_concat(Strings) when is_list(Strings) ->
  unicode:characters_to_binary(Strings).
str_trim(A) -> A.
hour(A) -> A.
sigma() -> 32982743927.
sigma(_) -> 32982743927.
day_of_week(_) -> 2.
