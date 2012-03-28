-module(service).

-behaviour(e2_service).

-export([start_link/0, start_link/1, stop/1]).

-export([handle_msg/3]).

start_link() ->
    start_link([]).

start_link(Options) ->
    e2_service:start_link(?MODULE, [], Options).

stop(Reason) ->
    e2_service:cast(?MODULE, {stop, Reason}).

handle_msg({stop, Reason}, noreply, State) ->
    {stop, Reason, State}.
