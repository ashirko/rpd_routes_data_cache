-module(rpd_geometry_data_cache).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([load_geoms/1, get_geom/1, get_all/0]).

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

-compile(export_all).
-record(state, {table_geom, table_trips}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  lager:info("start_link rpd_geometry_data_cache on node ~p", [node()]),
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

get_all()->
  gen_server:call({global,?SERVER}, get_all, 90000).

get_all_trips()->
  gen_server:call({global,?SERVER}, get_all_trips, 90000).

get_trip(RouteId)->
  gen_server:call({global,?SERVER}, {get_trip, RouteId}, 90000).

get_geom(RouteId)->
  gen_server:call({global,?SERVER}, {get_geom, RouteId}, 90000).

load_geoms(Routes)->
  gen_server:call({global, ?SERVER}, {load_geoms, Routes}, 1000000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  GeomTableId = ets:new(geom, [set]),
  TripsTableId = ets:new(trips, [bag]),
  {ok, #state{table_geom=GeomTableId, table_trips = TripsTableId}}.

handle_call({get_trip,Trip}, _From, #state{table_trips=TableId}=State) ->
  Res = ets:lookup(TableId, Trip),
  {reply, Res, State};
handle_call(get_all, _From, #state{table_geom=TableId}=State) ->
  All = ets:tab2list(TableId),
  {reply, All, State};
handle_call(get_all_trips, _From, #state{table_trips=TableId}=State) ->
  All = ets:tab2list(TableId),
  {reply, All, State};
handle_call({get_geom, RouteId}, _From, #state{table_geom=TableId, table_trips = TripsTable}=State) ->
  Result =
    case ets:lookup(TripsTable, RouteId) of
      []->
        {error, {route_not_found, RouteId}};
      Geom when is_list(Geom)->
        lager:info("Geom: ~p", [Geom]),
        Res = lists:foldl(fun({_,E},Acc)->
          T = ets:lookup(TableId, E),
          T ++ Acc
                          end, [], Geom),
        {ok,Res}
    end,
  {reply, Result, State};
handle_call({load_geoms, RouteIds}, _From, #state{table_geom=GeomTableId,
    table_trips = TripTableId}=State) ->
  ets:delete_all_objects(GeomTableId),
  ets:delete_all_objects(TripTableId),
  load_trips_id(TripTableId),
  lager:info("RouteIds: ~p", [RouteIds]),
  lists:foreach(fun(RouteId)->
    case ets:lookup(TripTableId, RouteId) of
      []->ok;
%%        lager:error("trips for route ~p were not found", [RouteId]);
      Trips->
%%        lager:info("Found trips for routeId ~p : Trips: ~p", [RouteId, Trips]),
        load_geometry(GeomTableId,Trips)
    end
                end, RouteIds),
  lager:info("finished foreach"),
  {reply, ok, State, ?RELOAD_TIMEOUT}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%handle_info(timeout, #state{table_geom=TableId}=State) ->
%%  {noreply, State, ?RELOAD_TIMEOUT};
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
  lager:info("Trips: ~p", [Trips]),
  true = ets:insert(TripTableId, Trips).

load_geometry(GeomTableId,Trips)->
  lists:foreach(fun({_,TripId})->
    case rnis_data_route_geometry_loader:load_geometry_for_route(TripId) of
      {ok, Geom}->
%%        lager:info("geometry data for ~p : ~p", [Trips, Geom]),
        true = ets:insert(GeomTableId, {TripId, Geom});
      {error, Reason}->
        lager:error("error while loading geometry data for ~p : ~p", [TripId, Reason])
    end
                end, Trips).

