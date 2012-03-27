-module(calc_handler_sup).

-behavior(e2_task_supervisor).

-export([start_link/0, start_handler/1]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, calc_handler, [registered]).

start_handler(Socket) ->
    e2_task_supervisor:start_task(?MODULE, [Socket]).
