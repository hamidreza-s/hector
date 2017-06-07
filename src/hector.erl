-module(hector).

-export([start/1]).
-export([route/2]).

-include_lib("eunit/include/eunit.hrl").
-include("hector.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start(hector_actor()) -> {ok, hector_actor()}.
start(Actor) ->
    hector_actor:start(Actor).

-spec route(hector_msg(), hector_path()) -> ok.
route(Msg, Path) ->
    hector_actor:route(Msg, Path).

%%%===================================================================
%%% @TODO: Unit tests
%%%===================================================================

route_test() ->

    ok = application:start(hector),

    ActorFunc1 = fun(Msg, State) ->
			 ?debugFmt("ACTOR1: ~p", [{Msg, State}]),
			 {ok, Msg, State}
		 end,
    ActorSpec1 = #hector_actor{name = "foo", handler = ActorFunc1},
    {ok, Actor1} = start(ActorSpec1),

    ActorFunc2 = fun(Msg, State) ->
			 ?debugFmt("ACTOR2: ~p", [{Msg, State}]),
			 {ok, Msg, State}
		 end,
    ActorSpec2 = #hector_actor{name = "bar", handler = ActorFunc2},
    {ok, Actor2} = start(ActorSpec2),

    ActorFunc3 = fun(Msg, State) ->
			 ?debugFmt("ACTOR3: ~p", [{Msg, State}]),
			 {ok, Msg, State}
		 end,
    ActorSpec3 = #hector_actor{name = "bat", handler = ActorFunc3},
    {ok, Actor3} = start(ActorSpec3),

    Path = [
	    {[Actor1, Actor2], [Actor3]},
	    {[Actor3], [Actor1, Actor2]}
	   ],

    ok = route("sample_message", Path),

    ok.
