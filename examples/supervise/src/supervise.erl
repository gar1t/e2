-module(supervise).

-export([start/0, permanent/0, temporary/0, transient/0]).

-import(error_logger, [info_msg/1, info_msg/2]).

start() ->
    application:start(sasl).

permanent() ->
    info_msg("Starting permanent_sup~n"),
    {ok, Sup} = permanent_sup:start_link(),

    timer:sleep(100),
    info_msg("Simulating normal service stop - WILL be restarted "
             "(permanent strategy)~n"),
    service:stop(normal),

    timer:sleep(100),
    info_msg("Simulating another normal service stop - will NOT be restarted "
             "(reached_max_restart_intensity)~n"),
    service:stop(normal),

    timer:sleep(100),
    info_msg("Stopping supervisor~n"),
    exit(Sup, shutdown),

    timer:sleep(100).

temporary() ->
    info_msg("Starting temporary_sup~n"),
    {ok, Sup} = temporary_sup:start_link(),

    timer:sleep(100),
    info_msg("Simulating service error - will NOT be started "
             "(temporary strategy)~n"),
    service:stop(error),

    timer:sleep(100),
    info_msg("Stopping supervisor~n"),
    exit(Sup, shutdown),

    timer:sleep(100).

transient() ->
    info_msg("Starting transient_sup~n"),
    {ok, Sup} = transient_sup:start_link(),

    timer:sleep(100),
    info_msg("Simulating service error - WILL be restarted "
             "(transient strategy)~n"),
    service:stop(error),

    timer:sleep(100),
    info_msg("Simulating normal service stop - will NOT be restarted "
             "(transient strategy)~n"),
    service:stop(normal),

    timer:sleep(100),
    info_msg("Stopping supervisor~n"),
    exit(Sup, shutdown),

    timer:sleep(100).
