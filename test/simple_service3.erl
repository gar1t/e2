%% Simple Service 3
%%
%% This service uses a variety of mechanisms to increment a list of
%% events and then exits.
%%
-module(simple_service3).

-export([start_link/2]).

-export([init/1, handle_msg/3, terminate/2]).

-record(state, {events, event_dest}).

start_link(Events, EventDest) ->
    e2_service:start_link(
      ?MODULE, #state{events=Events, event_dest=EventDest}).

init(State) ->
    %% Use a callback to manage the flow after init returns
    {ok, add_event(init, State), {handle_msg, post_init}}.

handle_msg(post_init, noreply, State) ->
    %% Send ourselves a message
    erlang:send(self(), a_msg),
    {noreply, add_event(post_init, State)};
handle_msg(a_msg, noreply, State) ->
    %% Stop to channel flow to terminate
    {stop, normal, add_event(a_msg, State)}.

terminate(Reason, State) ->
    %% Send our events to the event destination
    send_result(add_event({terminate, Reason}, State)).

add_event(Event, #state{events=Events}=State) ->
    State#state{events=[Event|Events]}.

send_result(#state{events=Events, event_dest=Dest}) ->
    erlang:send(Dest, {simple_service3_result, Events}).
