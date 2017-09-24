-module(rpd_routes_data_cache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rpd_routes_data_cache:start_link().

stop(_State) ->
    ok.
