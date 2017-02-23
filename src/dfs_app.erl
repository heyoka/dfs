%% Date: 19.02.17 - 19:37
%% Ⓒ 2017 LineMetrics GmbH
-module(dfs_app).
-author("Alexander Minichmair").

-behavior(application).
%% API
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
   dfs_sup:start_link().

stop(_State) ->
   ok.
