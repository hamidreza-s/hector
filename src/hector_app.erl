-module(hector_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    %% @TODO: make it configurable based on selected registry backend
    application:start(syn),

    hector_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
