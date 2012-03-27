-module({{task_module}}_sup).

-behavior(e2_task_supervisor).

-export([start_link/0, start_{{task_module}}/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
    e2_task_supervisor:start_link(?MODULE, {{task_module}}, [registered]).

start_{{task_module}}() ->
    e2_task_supervisor:start_task(?MODULE, []).
