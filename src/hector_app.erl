-module(hector_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    ok = lager:start(),
    ok = hector_registry:start(),

    hector_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
