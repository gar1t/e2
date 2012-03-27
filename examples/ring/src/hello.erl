-module(hello).

-behavior(e2_service).

-export([start_link/1, say/1, handle_msg/3]).

start_link(Prev) ->
    e2_service:start_link(?MODULE, Prev).

say(Hello) ->
    e2_service:cast(Hello, {hello, 1}).

handle_msg({hello, N}, noreply, first) ->
    e2_log:info("Said hello ~b times ~n", [N]),
    {stop, normal, first};
handle_msg({hello, N}, noreply, Prev) ->
    erlang:send(Prev, {hello, N + 1}),
    {stop, normal, Prev}.
