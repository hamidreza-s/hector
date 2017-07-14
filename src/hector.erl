-module(hector).

-export([start/1]).
-export([route/2]).
-export([result/2]).

-include_lib("eunit/include/eunit.hrl").
-include("hector.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start(hector_actor()) -> {ok, hector_actor()}.
start(Actor) ->
    hector_actor:start(Actor).

-spec route(hector_msg(), hector_path()) -> {ok, hector_ref()}.
route(Msg, Path) ->
    hector_actor:route(Msg, Path).

-spec result(hector_ref(), hector_path()) -> {ok, list(hector_msg())}.
result(Ref, Path) ->
    hector_actor:result(Ref, Path).

%%%===================================================================
%%% Unit tests
%%%===================================================================

route_test() ->

    ok = application:start(hector),

    ActorFunc1 = fun(Msg, State) -> {ok, Msg ++ "(A1)", State} end,
    ActorSpec1 = #hector_actor{name = "1", handler = ActorFunc1},
    {ok, Actor1} = start(ActorSpec1),

    ActorFunc2 = fun(Msg, State) -> {ok, Msg ++ "(A2)", State} end,
    ActorSpec2 = #hector_actor{name = "2", handler = ActorFunc2},
    {ok, Actor2} = start(ActorSpec2),

    ActorFunc3 = fun(Msg, State) -> {ok, Msg ++ "(A3)", State} end,
    ActorSpec3 = #hector_actor{name = "3", handler = ActorFunc3},
    {ok, Actor3} = start(ActorSpec3),

    ActorFunc4 = fun(Msg, State) -> {ok, Msg ++ "(A4)", State} end,
    ActorSpec4 = #hector_actor{name = "4", handler = ActorFunc4},
    {ok, Actor4} = start(ActorSpec4),

    Path1 = [
	     {[Actor1, Actor2], [Actor3]},
	     {[Actor3], [Actor1, Actor2]}
	    ],

    {ok, Ref1} = route("MSG1:", Path1),
    {ok, Result1} = result(Ref1, Path1),
    ?assertEqual(Result1, ["MSG1:(A1)(A3)(A1)","MSG1:(A2)(A3)(A2)"]),

    Path2 = [
    	     {[Actor1], [Actor2]},
    	     {[Actor2], [Actor3]},
    	     {[Actor3], [Actor4]},
    	     {[Actor4], [Actor3]},
    	     {[Actor3], [Actor2]},
    	     {[Actor2], [Actor1]}
    	    ],

    {ok, Ref2} = route("MSG1:", Path2),
    {ok, Result2} = result(Ref2, Path2),
    ?assertEqual(Result2, ["MSG1:(A1)(A2)(A3)(A4)(A3)(A2)(A1)"]),

    ok.
