-module(task_sup).

-behavior(e2_task_supervisor).

-export([start_link/0, start_link/1, start_task/3, start_task/4]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, task, [registered]).

start_link(RestartStrategy) ->
    e2_task_supervisor:start_link(?MODULE, {task, [RestartStrategy]}).

start_task(Name, ExitReason, ExitAfter) ->
    start_task(?MODULE, Name, ExitReason, ExitAfter).

start_task(Sup, Name, ExitReason, ExitAfter) ->
    e2_task_supervisor:start_task(Sup, [Name, ExitReason, ExitAfter]).
