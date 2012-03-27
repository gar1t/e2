-module(ping_server).

-behavior(e2_service).

-export([start_link/0, ping/0]).

-export([handle_msg/3]).

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

ping() ->
    e2_service:call(?MODULE, ping).

handle_msg(ping, _From, State) ->
    {reply, pong, State}.
