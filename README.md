Hector
=====

Hector is a distributed actor framework for Erlang and Elixir.

Concepts
-----

- Actor: A *vertex* in a graph which can send and receive messages and process them.
- Route: An *edge* between two actors which is used as a direction for sending a message from first actor the second one.
- Path: A *graph* which includes a list of pathes and is used for routing messages.
