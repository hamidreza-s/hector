-module(hector_registry_syn).

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
    ok = syn:start(),
    ok = syn:init(),
    ok.

-spec set_actor(hector_actor_name(), pid()) -> ok |
					       {taken_pid, hector_actor_name()} |
					       {taken_name, pid()} |
					       {error, not_consistent}.
set_actor(Name, PID) ->
    case syn:register(Name, PID) of
	ok ->
	    ok;
	{error, taken} ->
	    case get_actor_pid(Name) of
		{ok, TakenPID} ->
		    {take_pid, TakenPID};
		_ ->
		    {error, not_consistent}
	    end;
	{error, pid_already_registered} ->
	    case get_actor_name(PID) of
		{ok, TakenName} ->
		    {taken_name, TakenName};
		_ ->
		    {error, not_consistent}
	    end
    end.

-spec unset_actor(hector_actor_name()) -> ok | {error, not_found}.
unset_actor(Name) ->
    case syn:unregister(Name) of
	ok ->
	    ok;
	{error, undefined} ->
	    {error, not_found}
    end.

-spec get_actor_pid(hector_actor_name()) -> {ok, pid()} | {error, not_found}.
get_actor_pid(Name) ->
    case syn:find_by_key(Name) of
	undefined ->
	    {error, not_found};
	PID ->
	    {ok, PID}
    end.

-spec get_actor_name(pid()) -> {ok, hector_actor_name()} | {error, not_found}.
get_actor_name(PID) ->
    case syn:find_by_pid(PID) of
	undefined ->
	    {error, not_found};
	Name ->
	    {ok, Name}
    end.

%%%===================================================================
%%% @TODO: Unit tests
%%%===================================================================
