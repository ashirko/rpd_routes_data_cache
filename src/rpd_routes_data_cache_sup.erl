-module(rpd_routes_data_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("../../rnis_data/include/rnis_data.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    RoutesDataCache = {
        rpd_routes_data_cache,
        {rpd_routes_data_cache, start_link, []},
        permanent, 5000, worker, [rpd_routes_data_cache]},
    GeometryDataCache = {
        rpd_geometry_data_cache,
        {rpd_geometry_data_cache, start_link, []},
        permanent, 5000, worker, [rpd_geometry_data_cache]},
    ToStart = [RoutesDataCache, GeometryDataCache],
    {ok, {SupFlags, [ToStart]}}.

