-module(hector_actor).

-behaviour(gen_server).

-export([start/1]).
-export([route/2]).
-export([result/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include("hector.hrl").

-record(state, {
	  actor :: hector_actor(),
	  results = #{} :: #{hector_ref() => hector_msg()},
	  requests = #{} :: #{hector_ref() => pid()}
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

-spec route(hector_msg(), hector_path()) -> {ok, hector_ref()}.
route(Msg, [RootActors | RestPath]) ->
    %% @TODO: check if erlang reference works in distributed setup
    Ref = make_ref(),
    [gen_server:cast(PID, {route, Msg, Ref, RestPath}) || #hector_actor{pid = PID} <- RootActors],
    {ok, Ref}.

-spec result(hector_ref(), hector_path()) -> {ok, list(hector_msg())}.
result(Ref, Path) ->
    LastRoutes = lists:last(Path),
    Results = [gen_server:call(PID, {result, Ref}) || #hector_actor{pid = PID} <- LastRoutes],
    {ok, Results}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Actor]) ->
    {ok, #state{actor = Actor#hector_actor{pid = self()}}}.

handle_call({result, Ref}, From, #state{results = Results} = State) ->
    case maps:find(Ref, Results) of
	{ok, ResultMsg} ->
	    NewState = State#state{results = maps:remove(Ref, Results)},
	    {reply, ResultMsg, NewState};
	_ ->
	    NewState = State#state{requests = maps:put(Ref, From, Results)},
	    {noreply, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({route, Msg, Ref, Path}, State) ->

    %% @TODO: if a route has more than one sender,
    %% the receiver (current actor) must wait
    %% to get all the messages from all senders,
    %% then continue to handle them

    {ok, _NewMsg, NewState} = do_route(Msg, Ref, Path, State),
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

-spec do_route(hector_msg(), hector_ref(), hector_path(), state()) -> {ok, hector_msg(), state()}.
do_route(Msg, Ref, [Receivers | RestPath], State) ->
    Handler = (State#state.actor)#hector_actor.handler,

    {ok, NewMsg, NewState} = if
				 is_atom(Handler) ->
				     Handler:handle_msg(Msg, State);
				 is_function(Handler) ->
				     Handler(Msg, State);
				 true ->
				     {ok, Msg, State}
			     end,

    [gen_server:cast(PID, {route, NewMsg, Ref, RestPath}) || #hector_actor{pid = PID} <- Receivers],

    {ok, NewMsg, NewState};

do_route(Msg, Ref, [], #state{results = Results} = State) ->

    Handler = (State#state.actor)#hector_actor.handler,
    {ok, NewMsg, NewState} = if
				 is_atom(Handler) ->
				     Handler:handle_msg(Msg, State);
				 is_function(Handler) ->
				     Handler(Msg, State);
				 true ->
				     {ok, Msg, State}
			     end,


    %% @NOTE: here is the final route of the path
    %% so, we must keep the NewMsg as result in state
    %% and check the requests map if there is any pending request
    case maps:find(Ref, State#state.requests) of
	{ok, PID} ->
	    gen_server:reply(PID, NewMsg),
	    {ok, NewMsg, NewState};
	_ ->
	    {ok, NewMsg, NewState#state{results = maps:put(Ref, NewMsg, Results)}}
    end.
