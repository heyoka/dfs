%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Nov 2020 10:06
%%%-------------------------------------------------------------------
-module(test_helper).
-author("heyoka").
-compile(nowarn_export_all).
-compile(export_all).
%% API
-export([]).

string_data(ScriptFileName) ->
  {ok, Data} = file:read_file(ScriptFileName),
  binary_to_list(binary:replace(Data, <<"\\">>, <<>>, [global])).

