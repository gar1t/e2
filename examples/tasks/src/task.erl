-module(task).

-behavior(e2_task).

-export([start_link/3, handle_task/1]).

start_link(Name, ExitReason, ExitAfter) ->
    e2_task:start_link(?MODULE, {Name, ExitReason, ExitAfter}).

handle_task({Name, ExitReason, ExitAfter}) ->
    error_logger:info_msg("Task ~s starting~n", [Name]),
    timer:sleep(ExitAfter),
    error_logger:info_msg("Task ~s finished~n", [Name]),
    {stop, ExitReason}.
