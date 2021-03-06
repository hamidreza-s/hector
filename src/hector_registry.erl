-module(hector_registry).

-export([start/0]).
-export([set_actor/2]).
-export([unset_actor/1]).
-export([get_actor_pid/1]).
-export([get_actor_name/1]).

-include_lib("eunit/include/eunit.hrl").
-include("hector.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> ok.
start() ->
    ?HECTOR_REGISTRY:start().

-spec set_actor(hector_actor_name(), pid()) -> ok |
					       {taken_pid, hector_actor_name()} |
					       {taken_name, pid()} |
					       {error, not_consistent}.
set_actor(Name, PID) ->
    ?HECTOR_REGISTRY:set_actor(Name, PID).

-spec unset_actor(hector_actor_name()) -> ok | {error, not_found}.
unset_actor(Name) ->
    ?HECTOR_REGISTRY:unset_actor(Name).

-spec get_actor_pid(hector_actor_name()) -> {ok, pid()} | {error, not_found}.
get_actor_pid(Name) ->
    ?HECTOR_REGISTRY:get_actor_pid(Name).

-spec get_actor_name(pid()) -> {ok, hector_actor_name()} | {error, not_found}.
get_actor_name(PID) ->
    ?HECTOR_REGISTRY:get_actor_name(PID).

%%%===================================================================
%%% @TODO: Unit tests
%%%===================================================================
