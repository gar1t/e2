-module(ping).

-behavior(e2_application).

-export([start/0, ping/0]).

-export([init/0]).

start() ->
    e2_application:start_with_dependencies(ping).

ping() ->
    ping_server:ping().

init() ->
    {ok, [ping_server], [{max_restart, {4, 4}}]}.
