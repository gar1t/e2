-module(logger).

-behavior(e2_application).

-export([start/0, init/0]).

start() ->
    e2_application:start_with_dependencies(logger).

init() ->
    syslog_handler:install(),
    {ok, [{sample_process, [{restart, transient}]}]}.
