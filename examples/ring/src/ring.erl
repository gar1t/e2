-module(ring).

-export([start/1]).

start(N) when N > 0 ->
    e2_log:info("Starting processes~n"),
    {ok, Hello} = hello:start_link(first),
    Last = start_hello(Hello, N - 1),
    e2_log:info("Starting hello relay~n"),
    hello:say(Last).

start_hello(Last, 0) -> Last;
start_hello(Prev, N) ->
    {ok, Hello} = hello:start_link(Prev),
    start_hello(Hello, N - 1).
