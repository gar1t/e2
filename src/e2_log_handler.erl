%% ===================================================================
%% @author Garrett Smith <g@rre.tt>
%% @copyright 2011-2012 Garrett Smith
%%
%% @doc Behavior for custom log handlers.
%%
%% Refer to [https://github.com/gar1t/e2/tree/master/examples/logger
%% examples/logger] for a sample custom logger using this behavior.
%%
%% @end
%% ===================================================================

-module(e2_log_handler).

-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-export([behaviour_info/1]).

%% @private
behaviour_info(callbacks) -> [{handle_event, 2}].

-record(state, {mod, mod_state}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @private
init({Module, Args}) ->
    dispatch_init(Module, Args, init_state(Module)).

%% @private
handle_event(Event, State) ->
    dispatch_event(e2_log_event(Event), State).

%% @private
handle_call(_Msg, State) ->
    {ok, {error, not_handled}, State}.

%% @private
handle_info(_Msg, State) ->
    {ok, State}.

%% @private
terminate(Reason, State) ->
    dispatch_terminate(Reason, State).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_state(Module) ->
    #state{mod=Module}.

dispatch_init(Module, Args, State) ->
    case erlang:function_exported(Module, init, 1) of
        true ->
            handle_init_result(Module:init(Args), State);
        false ->
            handle_init_result({ok, Args}, State)
    end.

handle_init_result({ok, ModState}, State) ->
    {ok, set_mod_state(ModState, State)};
handle_init_result({error, Reason}, _State) ->
    {error, Reason};
handle_init_result(Other, _State) ->
    exit({bad_return_value, Other}).

e2_log_event({error, _Gleader, {_Pid, Format, Data}}) ->
    {error_msg, {Format, Data}};
e2_log_event({error_report, _Gleader, {_Pid, Type, Report}}) ->
    {error_report, {Type, Report}};
e2_log_event({warning_msg, _Gleader, {_Pid, Format, Data}}) ->
    {warning_msg, {Format, Data}};
e2_log_event({warning_report, _Gleader, {_Pid, Type, Report}}) ->
    {warning_report, {Type, Report}};
e2_log_event({info_msg, _Gleader, {_Pid, Format, Data}}) ->
    {info_msg, {Format, Data}};
e2_log_event({info_report, _Gleader, {_Pid, Type, Report}}) ->
    {info_report, {Type, Report}}.

dispatch_event(Event, #state{mod=Module, mod_state=ModState}=State) ->
    handle_event_result(Module:handle_event(Event, ModState), State).

handle_event_result({ok, ModState}, State) ->
    {ok, set_mod_state(ModState, State)};
handle_event_result(remove_handler, _State) ->
    remove_handler.

dispatch_terminate(Args, #state{mod=Module, mod_state=ModState}) ->
    case erlang:function_exported(Module, terminate, 2) of
        true -> Module:terminate(Args, ModState);
        false -> ok
    end.

set_mod_state(ModState, State) ->
    State#state{mod_state=ModState}.
