-module(hector_actor).

-behaviour(gen_server).

-export([start/1]).
-export([route/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include("hector.hrl").

-record(state, {
	  caller :: pid(),
	  actor :: hector_actor()}).

-define(SERVER, ?MODULE).
-define(BYTESIZE_ID, 8).

%%%===================================================================
%%% API
%%%===================================================================

-spec start(hector_actor()) -> {ok, hector_actor()}.
start(#hector_actor{name = Name} = Actor) ->
    {ok, PID} = gen_server:start_link(?MODULE, [Actor], []),
    ok = hector_registry:set_actor(Name, PID),
    {ok, Actor#hector_actor{pid = PID}}.

-spec route(hector_msg(), hector_path()) -> ok.
route(Msg, [{RootActor, _} | _] = Path) ->
    ok = gen_server:call(RootActor, {init_route, Msg, Path}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Actor]) ->
    {ok, #state{actor = Actor#hector_actor{pid = self()}}}.

handle_call({init_route, _Msg, _Path}, _From, State) ->
    %% @TODO: store from in state as caller
    %% @TODO: init routing
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({route, Msg, [{_CurrentActor, NextActor} | RestPath]}, #state{actor = Actor} = State) ->
    %% @TODO: check if current actor is valid
    Handler = Actor#hector_actor.handler,
    {ok, NewState} = Handler:handle_msg(Msg, State),
    erlang:send(NextActor, {route, Msg, RestPath}),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% @TODO: Unit tests
%%%===================================================================
