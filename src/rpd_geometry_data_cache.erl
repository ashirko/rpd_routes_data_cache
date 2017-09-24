-module(rpd_geometry_data_cache).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([load_geoms/1, get_geom/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-include_lib("../../rnis_data/include/rnis_data.hrl").
-define(SERVER, ?MODULE).
-define(RELOAD_TIMEOUT, 300000).
-define(INIT_TIMEOUT, 120000).

-record(state, {table_geom, table_trips}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  lager:info("start_link rpd_geometry_data_cache"),
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

get_geom(RouteId)->
  gen_server:call(?SERVER, {get_geom, RouteId}).

load_geoms(Routes)->
  gen_server:call(?SERVER, {load_geoms, Routes}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  GeomTableId = ets:new(geom, [set]),
  TripsTableId = ets:new(trips, [bag]),
  {ok, #state{table_geom=GeomTableId, table_trips = TripsTableId}}.

handle_call({get_geom, RouteId}, _From, #state{table_geom=TableId}=State) ->
  Result =
    case ets:lookup(TableId, RouteId) of
      []->
        {error, route_not_found};
      Geom when is_list(Geom)->
        {ok, Geom}
    end,
  {reply, Result, State};
handle_call({load_geoms, RouteIds}, _From, #state{table_geom=GeomTableId,
    table_trips = TripTableId}=State) ->
  ets:delete_all_objects(GeomTableId),
  ets:delete_all_objects(TripTableId),
  load_trips_id(TripTableId),
  lists:foreach(fun(RouteId)->
    case ets:lookup(TripTableId, RouteId) of
      []->
        lager:error("trips for route ~p were not found", [RouteId]);
      Trips->
        load_geometry(GeomTableId,Trips)
    end,
                end, TripsId),
  {reply, ok, State, ?RELOAD_TIMEOUT}.
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state{table_geom=TableId}=State) ->
  {noreply, State, ?RELOAD_TIMEOUT};
handle_info(_Info, State) ->
  {noreply, State, ?RELOAD_TIMEOUT}.

terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_trips_id(TripTableId)->
  Trips = rnis_data_trips_loader:load_data(),
  true = ets:insert(TripTableId, Trips).

load_geometry(GeomTableId,Trips)->
  lists:foreach(fun({_,TripId})->
    case rnis_data_route_geometry_loader:load_geometry_for_route(TripId) of
      {ok, Geom}->
        true = ets:insert(GeomTableId, {TripId, Geom});
      {error, Reason}->
        lager:error("error while loading geometry data for ~p : ~p", [TripId, Reason])
    end
                end, Trips).

