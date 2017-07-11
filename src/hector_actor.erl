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
	  actor :: hector_actor()
	 }).

-type state() :: #state{}.

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
route(Msg, [{RootActors, _} | _] = Path) ->
    [gen_server:cast(RootActor#hector_actor.pid, {route, Msg, Path})
     ||	RootActor <- RootActors],
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Actor]) ->
    {ok, #state{actor = Actor#hector_actor{pid = self()}}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({route, Msg, Path}, State) ->

    %% @TODO
    %% If a route has more than one sender,
    %% the receiver (current actor) must wait
    %% to get all the messages from all senders,
    %% then continue to handle them

    {ok, _NewMsg, NewState} = do_route(Msg, Path, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% internal functions
%%%===================================================================

-spec do_route(hector_msg(), hector_path(), state()) -> {ok, state()}.
do_route(Msg, [{_Senders, Receivers} | RestPath], State) ->
    Handler = (State#state.actor)#hector_actor.handler,
    {ok, NewMsg, NewState} = if
				 is_atom(Handler) ->
				     Handler:handle_msg(Msg, State);
				 is_function(Handler) ->
				     Handler(Msg, State);
				 true ->
				     {ok, Msg, State}
			     end,

    [gen_server:cast(Actor#hector_actor.pid, {route, NewMsg, RestPath})
     || Actor <- Receivers],

    {ok, NewMsg, NewState};

do_route(Msg, [], State) ->

    Handler = (State#state.actor)#hector_actor.handler,
    {ok, NewMsg, NewState} = if
				 is_atom(Handler) ->
				     Handler:handle_msg(Msg, State);
				 is_function(Handler) ->
				     Handler(Msg, State);
				 true ->
				     {ok, Msg, State}
			     end,

    {ok, NewMsg, NewState}.
