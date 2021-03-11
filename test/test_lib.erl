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
