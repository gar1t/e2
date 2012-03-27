-module(pubsub).

-behavior(e2_application).

-export([start/0]).

-export([init/0]).

start() ->
    e2_application:start_with_dependencies(pubsub).

init() ->
    {ok, [broker]}.
