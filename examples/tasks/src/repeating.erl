-module(repeating).

-behavior(e2_task).

-export([start_link/1, start_link/2, reset/1]).

-export([init/1, handle_task/1, handle_msg/3]).

start_link({options, Options}) ->
    e2_task:start_link(?MODULE, 1, Options);
start_link({init, Timing}) ->
    e2_task:start_link(?MODULE, {1, Timing}, []).

start_link(Options, Timing) ->
    e2_task:start_link(?MODULE, {1, Timing}, Options).

init({N, Timing}) -> {ok, N, Timing};
init(N) -> {ok, N}.

reset(Task) ->
    e2_task:cast(Task, reset).

handle_task(N) ->
    io:format(user, "~f - exec ~b~n", [timestamp(), N]),
    {repeat, N + 1}.

handle_msg(reset, _From, _N) ->
    {reply, ok, 1}.

timestamp() ->
    {M, S, U} = erlang:now(),
    M * 1000000 + S + U / 1000000.
