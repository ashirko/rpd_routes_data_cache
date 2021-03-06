-module(rpd_routes_data_cache).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include_lib("../../rnis_data/include/rnis_data.hrl").
-define(SERVER, ?MODULE).
-define(INIT_TIMEOUT, 60000).
-define(RELOAD_TIMEOUT, 14400000).
-define(WAIT_GEOM_TIMEOUT, ?INIT_TIMEOUT).
-record(state, {routes, timer_ref}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Routes = rnis_data_routes_loader:load_data(),
  ReloadTimeout = application:get_env(rpd_routes_data_cache,
    reload_routes_timeout, ?RELOAD_TIMEOUT),
  {ok,Ref} = timer:send_after(ReloadTimeout, reload),
  {ok, #state{routes = Routes, timer_ref = Ref}, ?INIT_TIMEOUT}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state{routes = Routes} = State) ->
  RoutesId = lists:usort(lists:map(fun(#route_descr{id = Id}) ->
    Id end, Routes)),
  lager:info("start load geoms"),
  rpd_geometry_data_cache:load_geoms(RoutesId),
  lager:info("finish load geoms"),
  {Reg, NotReg} = send_data_to_atts(Routes),
  lager:info("num of reg atts: ~p", [length(Reg)]),
  lager:info("num of not reg atts: ~p", [length(NotReg)]),
  {noreply, State};
handle_info(reload, #state{timer_ref = Ref}=State) ->
  timer:cancel(Ref),
  Routes = rnis_data_routes_loader:load_data(),
  ReloadTimeout = application:get_env(rpd_routes_data_cache,
    reload_routes_timeout, ?RELOAD_TIMEOUT),
  {ok,NewRef} = timer:send_after(ReloadTimeout, reload),
  {noreply, State#state{routes = Routes, timer_ref = NewRef}, 0};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_data_to_atts(Routes)->
  lists:foldl(fun(Route, {Acc,ErrAcc})->
    Id = Route#route_descr.att_id,
    case rnis_data_att_cache:is_register(Id) of
      true->
        rnis_data_att_fsm:send_route_data(Id, Route),
        timer:sleep(15),
        {[Id|Acc], ErrAcc};
      false->
        {Acc, [{is_not_registred, Id} | ErrAcc]}
    end
              end, {[],[]}, Routes).