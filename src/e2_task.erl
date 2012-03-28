%% ===================================================================
%% @author Garrett Smith <g@rre.tt>
%% @copyright 2011-2012 Garrett Smith
%%
%% @doc e2 task behavior.
%%
%% Tasks are e2_service behaviors that run actively after started.
%%
%% A task may be delayed after started.
%%
%% A task may repeat.
%%
%% For more information see [http://e2project.org/tasks.html e2
%% tasks] documentation.
%% @end
%% ===================================================================

-module(e2_task).

-behavior(e2_service).

-export([start_link/2, start_link/3, run_once/1,
         call/2, call/3, cast/2]).

-export([init/1, handle_msg/3, terminate/2]).

-export([behaviour_info/1]).

%% @private
behaviour_info(callbacks) -> [{handle_task, 1}].

-record(state, {mod, mod_state, handle_msg, start, repeat}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Start a linked task.
%% @equiv start_link(Module, Args, [])
%% @end
%%--------------------------------------------------------------------

start_link(Module, Args) ->
    start_link(Module, Args, []).

%%--------------------------------------------------------------------
%% @doc Start a linked task.
%%
%% Refer to {@link e2_service:start_link/3} for details.
%%
%% @spec start_link(Module, Args, Options) -> {ok, Pid} | {error, Reason}
%% @end
%%--------------------------------------------------------------------

start_link(Module, Args, Options) ->
    {ServiceOpts, TaskOpts} = e2_service_impl:split_options(Module, Options),
    e2_service:start_link(?MODULE, {Module, Args, TaskOpts}, ServiceOpts).

%%--------------------------------------------------------------------
%% @doc Runs the task.
%%
%% If the task is repeating, this will not cause subsequent task runs.
%%
%% @spec (Task) -> ok
%% Task = atom() | pid()
%% @end
%%--------------------------------------------------------------------

run_once(Task) ->
    e2_service:cast(Task, '$run').

%%--------------------------------------------------------------------
%% @doc Sends a message to a task and waits for a reply.
%% @equiv call(Task, Msg, infinity)
%% @end
%%--------------------------------------------------------------------

call(Task, Msg) ->
    e2_service:call(Task, Msg).

%%--------------------------------------------------------------------
%% @doc Sends a message to a task and waits for a reply.
%% @spec (Task, Msg, Timeout) -> Reply
%% Task = atom() | pid()
%% Msg = term()
%% Timeout = integer() | infinity
%% Reply = term()
%% @end
%%--------------------------------------------------------------------

call(Task, Msg, Timeout) ->
    e2_service:call(Task, Msg, Timeout).

%%--------------------------------------------------------------------
%% @doc Sends a message to a task without waiting for a reply.
%% @spec (Task, Msg) -> ok
%% Task = atom() | pid()
%% Msg = term()
%% @end
%%--------------------------------------------------------------------

cast(Task, Msg) ->
    e2_service:cast(Task, Msg).

%%%===================================================================
%%% e2_service callbacks
%%%===================================================================

%% @private
init({Module, Args, TaskOpts}) ->
    e2_service_impl:set_trap_exit(Module),
    dispatch_init(Module, Args, TaskOpts, init_state(Module)).

%% @private
handle_msg('$task', noreply, State) ->
    dispatch_handle_task(set_start(State));
handle_msg('$run', noreply, State) ->
    dispatch_handle_run(State);
handle_msg(_Msg, _From, #state{handle_msg=false}=State) ->
    {noreply, State};
handle_msg(Msg, From, #state{mod=Module, mod_state=ModState0}=State) ->
    {Result, ModState} =
        e2_service_impl:dispatch_handle_msg(Module, Msg, From, ModState0),
    e2_service_impl:handle_msg_result(Result, set_mod_state(ModState, State)).

%% @private
terminate(Reason, State) ->
    dispatch_terminate(Reason, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_state(Module) ->
    #state{mod=Module,
           handle_msg=erlang:function_exported(Module, handle_msg, 3)}.

dispatch_init(Module, Args, Options, State) ->
    handle_init_result(
      dispatch_init(Module, Args), timing_spec(Options), State).

dispatch_init(Module, Args) ->
    case erlang:function_exported(Module, init, 1) of
        true -> Module:init(Args);
        false -> {ok, Args}
    end.

timing_spec(Options) ->
    case {proplists:get_value(delay, Options),
          proplists:get_value(repeat, Options)}
    of
        {undefined, undefined} -> 0;
        {undefined, Repeat} -> {0, Repeat};
        {Delay, undefined} -> Delay;
        {Delay, Repeat} -> {Delay, Repeat}
    end.

handle_init_result({ok, ModState}, OptionsTiming, State) ->
    handle_init_result({ok, ModState, OptionsTiming}, State);
handle_init_result({ok, ModState, Timing}, _OptionsTiming, State) ->
    handle_init_result({ok, ModState, Timing}, State);
handle_init_result({stop, Reason}, _OptionsTiming, _State) ->
    {stop, Reason};
handle_init_result(ignore, _OptionsTiming, _State) ->
    ignore;
handle_init_result(Other, _OptionsTiming, _State) ->
    error({bad_return_value, Other}).

handle_init_result({ok, ModState, {0, Repeat}}, State) ->
    {ok, State#state{repeat=Repeat, mod_state=ModState},
     {handle_msg, '$task'}};
handle_init_result({ok, ModState, {Delay, Repeat}}, State) ->
    erlang:send_after(Delay, self(), '$task'),
    {ok, State#state{repeat=Repeat, mod_state=ModState}};
handle_init_result({ok, ModState, 0}, State) ->
    {ok, State#state{mod_state=ModState}, {handle_msg, '$task'}};
handle_init_result({ok, ModState, Delay}, State) ->
    erlang:send_after(Delay, self(), '$task'),
    {ok, State#state{mod_state=ModState}}.

dispatch_handle_task(#state{mod=Module, mod_state=ModState}=State) ->
    handle_task_result(Module:handle_task(ModState), State).

handle_task_result({repeat, ModState}, State) ->
    handle_task_repeat(repeat_delay(State), set_mod_state(ModState, State));
handle_task_result({repeat, ModState, Delay}, State) ->
    handle_task_repeat(Delay, set_mod_state(ModState, State));
handle_task_result({stop, Reason}, State) ->
    {stop, Reason, State};
handle_task_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_task_result({wait_for_msg, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_task_result({wait_for_msg, ModState, Timeout}, State) ->
    {noreply, set_mod_state(ModState, State), Timeout};
handle_task_result({hibernate, ModState}, State) ->
    {noreply, set_mod_state(ModState, State), hibernate};
handle_task_result(Other, _State) ->
    exit({bad_return_value, Other}).

handle_task_repeat(0, State) ->
    {noreply, State, {handle_msg, '$task'}};
handle_task_repeat(Delay, State) ->
    erlang:send_after(Delay, self(), '$task'),
    {noreply, State}.

handle_run_result({repeat, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_run_result({repeat, ModState, _Delay}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_run_result({stop, Reason}, State) ->
    {stop, Reason, State};
handle_run_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_run_result({wait_for_msg, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_run_result({wait_for_msg, ModState, Timeout}, State) ->
    {noreply, set_mod_state(ModState, State), Timeout};
handle_run_result({hibernate, ModState}, State) ->
    {noreply, set_mod_state(ModState, State), hibernate};
handle_run_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_handle_run(#state{mod=Module, mod_state=ModState}=State) ->
    handle_run_result(Module:handle_task(ModState), State).

dispatch_terminate(Reason, #state{mod=Module, mod_state=ModState}) ->
    e2_service_impl:dispatch_terminate(Module, Reason, ModState).

repeat_delay(#state{repeat=undefined}) -> 0;
repeat_delay(#state{repeat=0}) -> 0;
repeat_delay(#state{repeat=Interval, start=Start}) ->
    Now = timestamp(),
    ((Now - Start) div Interval + 1) * Interval + Start - Now.

set_mod_state(ModState, State) ->
    State#state{mod_state=ModState}.

set_start(#state{start=undefined}=State) ->
    State#state{start=timestamp()};
set_start(State) -> State.

timestamp() ->
    {M, S, U} = erlang:now(),
    M * 1000000000 + S * 1000 + U div 1000.
