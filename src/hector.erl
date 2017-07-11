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
%%% Unit tests
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

    ActorFunc4 = fun(Msg, State) ->
			 ?debugFmt("ACTOR4: ~p", [{Msg, State}]),
			 {ok, Msg, State}
		 end,
    ActorSpec4 = #hector_actor{name = "ban", handler = ActorFunc4},
    {ok, Actor4} = start(ActorSpec4),

    Path1 = [
	     {[Actor1, Actor2], [Actor3]},
	     {[Actor3], [Actor1, Actor2]}
	    ],

    ok = route("sample_message_1", Path1),

    Path2 = [
	     {[Actor1], [Actor2]},
	     {[Actor2], [Actor3]},
	     {[Actor3], [Actor4]},
	     {[Actor4], [Actor3]},
	     {[Actor3], [Actor2]},
	     {[Actor2], [Actor1]}
	    ],

    ok = route("sample_message_2", Path2),

    ok.
