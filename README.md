Hector
=====

Hector is an actor-based message routing library for Erlang and Elixir.

Concepts
-----

- **Actor**: A *vertex* in a graph which can send and receive messages and process them.
- **Route**: An *edge* between two actors which is used as a direction for sending a message from first actor the second one.
- **Path**: A *graph* which includes a list of routes and is used for routing messages.

Actor Handler
-----

Actor handler can be:

- **Module**: It must be a module compatible with `hector_actor` behaviour
implementing `init`, `terminate`, and `handle_msg(Msg, State)` which must returns
`{ok, Msg, State}` in response to let routing continue.

- **Function**: It must be a function that gets `Msg` and `State` as arguments and
also must return `{ok, Msg, State}` in response to let routing continue.

Routing Patterns
-----

- One route from actor `a` to actor `b`:
```
[[a], [b]]
```
- Two routes from actor `a` to both actor `b` and `c`:
```
[[a], [b, c]]
```
- Two routes from both actor `a` and `b` to actor `c`:
```
[[a, b], [c]]
```
