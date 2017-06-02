-module(hector_utils).

-export([generate_id/0]).

-include_lib("eunit/include/eunit.hrl").
-include("hector.hrl").

-spec generate_id() -> hector_id().
generate_id() ->
    crypto:strong_rand_bytes(16).
