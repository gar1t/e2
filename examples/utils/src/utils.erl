-module(utils).

-behavior(e2_application).

-export([start/0, init/0]).

start() ->
    e2_application:start_with_dependencies(utils).

init() ->
    {ok, [sequence]}.
