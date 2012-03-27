-module(sequence).

-behavior(e2_service).

-export([start_link/0, next/0, reset/0, reset/1]).

-export([handle_msg/3]).

start_link() ->
    e2_service:start_link(?MODULE, 1, [registered]).

next() ->
    e2_service:call(?MODULE, next).

reset() ->
    reset(1).

reset(Start) ->
    e2_service:cast(?MODULE, {reset, Start}).

handle_msg(next, _From, Next) ->
    {reply, Next, Next + 1};
handle_msg({reset, Next}, noreply, _OldNext) ->
    {noreply, Next}.
