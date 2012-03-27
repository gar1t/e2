%% Simple Service 2
%%
%% This service is also very simple -- but adds a couple twists. First,
%% there's a slightly complex service init process: a string representation
%% of a number is converted to a number. This could be any initialization
%% within the context of the service process though. Second, the service
%% implements a function (incr_val/2) using a cast.
%%
-module(simple_service2).

-export([start_link/1, get_val/1, incr_val/2]).

-export([init/1, handle_msg/3]).

start_link(NumAsStr) when is_list(NumAsStr) ->
    e2_service:start_link(?MODULE, NumAsStr).

get_val(Service) ->
    e2_service:call(Service, val).

incr_val(Service, I) ->
    e2_service:cast(Service, {incr, I}).

init(NumAsStr) ->
    {ok, list_to_integer(NumAsStr)}.

handle_msg(val, _From, Val) ->
    {reply, Val, Val};
handle_msg({incr, I}, noreply, Val) ->
    {noreply, Val + I}.

