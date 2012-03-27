-module(mfa_task_sup).

-behavior(e2_task_supervisor).

-export([start_link/0, start_task/3]).

start_link() ->
    e2_task_supervisor:start_link(
      ?MODULE, {erlang, apply, []}, [registered]).

start_task(M, F, A) ->
    e2_task_supervisor:start_task(?MODULE, [M, F, A]).

