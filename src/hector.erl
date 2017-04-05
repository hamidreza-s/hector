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
