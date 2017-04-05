-module(hector_registry).

-export([set_actor/2]).
-export([unset_actor/1]).
-export([get_actor_pid/1]).
-export([get_actor_name/1]).

-include_lib("eunit/include/eunit.hrl").
-include("hector.hrl").

-define(BACKEND, hector_registry_syn).

%%%===================================================================
%%% API
%%%===================================================================

-spec set_actor(hector_actor_name(), pid()) -> ok |
					       {taken_pid, hector_actor_name()} |
					       {taken_name, pid()} |
					       {error, not_consistent}.
set_actor(Name, PID) ->
    ?BACKEND:set_actor(Name, PID).

-spec unset_actor(hector_actor_name()) -> ok | {error, not_found}.
unset_actor(Name) ->
    ?BACKEND:unset_actor(Name).

-spec get_actor_pid(hector_actor_name()) -> {ok, pid()} | {error, not_found}.
get_actor_pid(Name) ->
    ?BACKEND:get_actor_pid(Name).

-spec get_actor_name(pid()) -> {ok, hector_actor_name()} | {error, not_found}.
get_actor_name(PID) ->
    ?BACKEND:get_actor_name(PID).

%%%===================================================================
%%% @TODO: Unit tests
%%%===================================================================
