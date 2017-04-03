%%%===================================================================
%%%Records
%%%===================================================================

-record(hector_spec, {
	  name :: hector_name(),
	  handler :: hector_handler(),
	  opts :: hector_opts()
	 }).

-record(hector_opts, {
	  auto = false :: boolean(),
	  blocking = false :: boolean()
	 }).

-record(hector_req, {
	  body :: any(),
	  timeout :: milliseconds()
	 }).

-record(hector_res, {
	  body :: any(),
	  elapsed :: milliseconds()
	 }).

%%%===================================================================
%%% Hector Types
%%%===================================================================

-type hector_spec() :: #hector_spec{}.
-type hector_opts() :: #hector_opts{}.

-type hector_name() :: atom() | string() | integer().
-type hector_ref() :: pid().
-type hector_handler() :: module().

-type hector_exec() :: parallel | sequential | shuffle.
-type hector_path() :: [{hector_exec(), [hector_name()]}].
-type hector_future() :: fun(({ok, hector_res()} | {nok, hector_err()}) -> ok).

-type hector_err() :: any().
-type hector_msg() :: any().

-type hector_req() :: #hector_req{}.
-type hector_res() :: #hector_res{}.

%%%===================================================================
%%% Misc Types
%%%===================================================================

-type milliseconds() :: integer().

%%%===================================================================
%%% Macros
%%%===================================================================
