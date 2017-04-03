-module(hector).

-export([start/1]).
-export([start/2]).
-export([call/2]).
-export([call/3]).
-export([cast/2]).

-include("hector.hrl").

-spec start(hector_spec()) -> {ok, hector_name(), hector_ref()}.
start(Spec) ->
    start(Spec, #hector_opts{}).

-spec start(hector_spec(), hector_opts()) -> {ok, hector_name(), hector_ref()}.
start(_Spec, _Opts) ->
    %% @TODO: implement it.
    ok.

-spec call(hector_req(), hector_path()) -> {ok, hector_res()} | {nok, hector_err()}.
call(_Req, _Path) ->
    %% @TODO: implement it.
    ok.

-spec call(hector_req(), hector_path(), hector_future()) -> ok.
call(_Req, _Path, _Future) ->
    %% @TODO: implement it.
    ok.

-spec cast(hector_msg(), hector_path()) -> ok.
cast(_Msg, _Path) ->
    %% @TODO: implement it.
    ok.
