%% ===================================================================
%% @author Garrett Smith <g@rre.tt>
%% @copyright 2011-2012 Garrett Smith
%%
%% @doc Utility module for implementing custom e2 tasks or task
%% behavior within a non-task service.
%% @see e2_service_impl
%% @end
%% ===================================================================

-module(e2_task_impl).

-export([start_repeat/2, start_repeat/3, next_repeat/1]).

-record(repeat, {target, interval, start, msg}).

-define(is_delay(D), (is_integer(D) andalso D >= 0)).
-define(is_interval(I), (is_integer(I) andalso I >= 0)).

start_repeat(Interval, Msg) ->
    start_repeat(Interval, Interval, Msg).

start_repeat(Delay, Interval, Msg)
  when ?is_delay(Delay), ?is_interval(Interval) ->
    Target = self(),
    erlang:send_after(Delay, Target, Msg),
    #repeat{target=Target, interval=Interval, msg=Msg}.

next_repeat(#repeat{start=undefined}=R) ->
    next_repeat(R#repeat{start=timestamp()});
next_repeat(#repeat{target=Target, msg=Msg}=Repeat) ->
    erlang:send_after(delay(Repeat), Target, Msg),
    Repeat.

delay(#repeat{interval=0}) -> 0;
delay(#repeat{interval=Interval, start=Start}) ->
    Now = timestamp(),
    ((Now - Start) div Interval + 1) * Interval + Start - Now.

timestamp() ->
    {M, S, U} = erlang:now(),
    M * 1000000000 + S * 1000 + U div 1000.
