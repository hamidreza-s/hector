%%%===================================================================
%%% Macros
%%%===================================================================

-define(REQUIRED(Field), error({field_required, Field})).

%%%===================================================================
%%% Records
%%%===================================================================

-record(hector_actor, {
	  name = ?REQUIRED(name) :: hector_actor_name(),
	  handler = ?REQUIRED(handler) :: hector_actor_handler(),
	  auto = false :: boolean(),
	  blocking = false :: boolean(),
	  node = node() :: node(),
	  pid :: pid()
	 }).

%%%===================================================================
%%% Base types
%%%===================================================================

-type hector_actor_name() :: string() | non_neg_integer().
-type hector_actor_handler() :: module() | function().

-type hector_actor() :: #hector_actor{}.
-type hector_actors() :: list(hector_actor()).
-type hector_route() :: {hector_actors(), hector_actors()}.
-type hector_path() :: list(hector_route()).

-type hector_id() :: any().
-type hector_msg() :: any().
-type hector_ref() :: reference().
-type hector_err() :: timeout
		    | bad_request
		    | field_required
		    | duplicated_name
		    | duplicated_id
		    | any().

%%%===================================================================
%%% Base types (based on graph terminology)
%%%===================================================================

-type hector_vertex() :: hector_actor().
-type hector_edge() :: hector_route().
-type hector_graph() :: hector_path().

%%%===================================================================
%%% Misc types
%%%===================================================================

-type milliseconds() :: integer().
