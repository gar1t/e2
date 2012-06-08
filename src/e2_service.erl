%% ===================================================================
%% @author Garrett Smith <g@rre.tt>
%% @copyright 2011-2012 Garrett Smith
%%
%% @doc e2 service behavior.
%%
%% e2 services are the base process type in e2.
%%
%% e2 services provide the same functionality as
%% [http://www.erlang.org/doc/man/gen_server.html gen_server] but with
%% a simplified interface.
%%
%% For more information see [http://e2project.org/services.html e2
%% services] documentation.
%% @end
%% ===================================================================

-module(e2_service).

-behaviour(gen_server).

-export([start_link/2, start_link/3, call/2, call/3, cast/2, reply/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([behaviour_info/1]).

%% @private
behaviour_info(callbacks) -> [{handle_msg, 3}].

-record(state, {mod, mod_state, timeout_msg}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts an e2 service.
%% @equiv start_link(Module, Args)
%% @end
%%--------------------------------------------------------------------

start_link(Module, Args) ->
    start_link(Module, Args, []).

%%--------------------------------------------------------------------
%% @doc Starts an e2 service.
%%
%% Module is the service callback module.
%%
%% If `init/1' is exported, Args is the argument passed to the function
%% when the service is initialized. If `init/1' is not exported, Args
%% is the initial service state.
%%
%% If `registered' or `{registered, Name}' is provided as an option, the
%% service will be registered. If not specified, the registered name
%% defaults to Module.
%%
%% @spec start_link(Module, Args, Options) -> {ok, Pid} | {error, Reason}
%% Module = atom()
%% Args = term()
%% Options = [Option]
%% Option = registered | {registered, Name}
%% Name = atom()
%% @end
%%--------------------------------------------------------------------

start_link(Module, Args, Options) ->
    start_gen_server(server_name(Module, Options), Module, Args, Options).

%%--------------------------------------------------------------------
%% @doc Sends a message to a service and waits for a reply.
%% @equiv call(ServiceRef, Msg, infinity)
%% @end
%%--------------------------------------------------------------------

call(ServiceRef, Msg) ->
    call(ServiceRef, Msg, infinity).

%%--------------------------------------------------------------------
%% @doc Sends a message to a service and waits for a reply.
%% @spec (ServiceRef, Msg, Timeout) -> Reply
%% ServiceRef = atom() | pid()
%% Msg = term()
%% Timeout = integer() | infinity
%% Reply = term()
%% @end
%%--------------------------------------------------------------------

call(ServiceRef, Msg, Timeout) ->
    gen_server:call(ServiceRef, {'$call', Msg}, Timeout).

%%--------------------------------------------------------------------
%% @doc Sends a message to a service without waiting for a reply.
%% @spec (ServiceRef, Msg) -> ok
%% ServiceRef = atom() | pid()
%% Msg = term()
%% @end
%%--------------------------------------------------------------------

cast(ServiceRef, Msg) ->
    gen_server:cast(ServiceRef, {'$cast', Msg}).

%%--------------------------------------------------------------------
%% @doc Used by service implementations to reply to a message.
%%
%% Client is the `From' argument from the request `handle_msg/3'
%% call.
%%
%% @spec reply(Client, Reply) -> any()
%% Client = term()
%% Reply = term()
%% @end
%%--------------------------------------------------------------------

reply(Client, Reply) ->
    gen_server:reply(Client, Reply).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Module, Args}) ->
    maybe_trap_exit(Module),
    dispatch_init(Module, Args, init_state(Module)).

%% @private
handle_call({'$call', Msg}, From, State) ->
    dispatch_call(Msg, From, State).

%% @private
handle_cast({'$cast', Msg}, State) ->
    dispatch_cast(Msg, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(Msg, State) ->
    dispatch_info(Msg, State).

%% @private
terminate(Reason, State) ->
    dispatch_terminate(Reason, State).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

server_name(Module, Options) ->
    case proplists:get_value(registered, Options) of
        undefined -> unregistered;
        true -> {registered, Module};
        Name when is_atom(Name) -> {registered, Name};
        Other -> error({badarg, Other})
    end.

start_gen_server(unregistered, Module, Args, Options) ->
    gen_server:start_link(
      ?MODULE, {Module, Args}, gen_server_options(Options));
start_gen_server({registered, Name}, Module, Args, Options) ->
    gen_server:start_link(
      {local, Name}, ?MODULE, {Module, Args}, gen_server_options(Options)).

%% TODO: What do we want to pass through to gen_server?
gen_server_options(_Options) -> [].

init_state(Module) when is_atom(Module) ->
    #state{mod=Module}.

maybe_trap_exit(Module) ->
    case erlang:function_exported(Module, terminate, 2) of
        true -> process_flag(trap_exit, true);
        false -> ok
    end.

dispatch_init(Module, Args, State) ->
    case erlang:function_exported(Module, init, 1) of
        true ->
            handle_init_result(Module:init(Args), State);
        false ->
            handle_init_result({ok, Args}, State)
    end.

handle_init_result({ok, ModState}, State) ->
    {ok, set_mod_state(ModState, State)};
handle_init_result({ok, ModState, {handle_msg, Msg}}, State) ->
    {ok, set_timeout_msg(Msg, set_mod_state(ModState, State)), 0};
handle_init_result({ok, ModState, {timeout, Timeout}}, State) ->
    {ok, set_mod_state(ModState, State), Timeout};
handle_init_result({ok, ModState, hibernate}, State) ->
    {ok, set_mod_state(ModState, State), hibernate};
handle_init_result({stop, Reason}, _State) ->
    {stop, Reason};
handle_init_result(ignore, _State) ->
    ignore;
handle_init_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_call(Msg, From, #state{mod=Module, mod_state=ModState}=State) ->
    handle_call_result(dispatch_msg(Module, Msg, From, ModState), State).

handle_call_result({reply, Reply, ModState}, State) ->
    {reply, Reply, set_mod_state(ModState, State)};
handle_call_result({noreply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_call_result({reply, Reply, ModState, {handle_msg, Msg}}, State) ->
    {reply, Reply, set_timeout_msg(Msg, set_mod_state(ModState, State)), 0};
handle_call_result({reply, Reply, ModState, {timeout, Timeout}}, State) ->
    {reply, Reply, set_mod_state(ModState, State), Timeout};
handle_call_result({reply, Reply, ModState, hibernate}, State) ->
    {reply, Reply, set_mod_state(ModState, State), hibernate};
handle_call_result({noreply, ModState, {handle_msg, Msg}}, State) ->
    {noreply, set_timeout_msg(Msg, set_mod_state(ModState, State)), 0};
handle_call_result({noreply, ModState, {timeout, Timeout}}, State) ->
    {noreply, set_mod_state(ModState, State), Timeout};
handle_call_result({noreply, ModState, hibernate}, State) ->
    {noreply, set_mod_state(ModState, State), hibernate};
handle_call_result({stop, Reason}, State) ->
    {stop, Reason, State};
handle_call_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_call_result({stop, Reason, Reply, ModState}, State) ->
    {stop, Reason, Reply, set_mod_state(ModState, State)};
handle_call_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_cast(Msg, #state{mod=Module, mod_state=ModState}=State) ->
    handle_cast_result(dispatch_msg_noreply(Module, Msg, ModState), State).

handle_cast_result({noreply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_cast_result({reply, _Reply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_cast_result({noreply, ModState, {handle_msg, NextMsg}}, State) ->
    {noreply, set_timeout_msg(NextMsg, set_mod_state(ModState, State)), 0};
handle_cast_result({noreply, ModState, {timeout, Timeout}}, State) ->
    {noreply, set_mod_state(ModState, State), Timeout};
handle_cast_result({noreply, ModState, hibernate}, State) ->
    {noreply, set_mod_state(ModState, State), hibernate};
handle_cast_result({reply, _Reply, ModState, {handle_msg, Msg}}, State) ->
    {noreply, set_timeout_msg(Msg, set_mod_state(ModState, State)), 0};
handle_cast_result({reply, _Reply, ModState, {timeout, Timeout}}, State) ->
    {noreply, set_mod_state(ModState, State), Timeout};
handle_cast_result({reply, _Reply, ModState, hibernate}, State) ->
    {noreply, set_mod_state(ModState, State), hibernate};
handle_cast_result({stop, Reason}, State) ->
    {stop, Reason, State};
handle_cast_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_cast_result({stop, Reason, _Reply, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_cast_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_info(timeout, #state{mod=Module, mod_state=ModState,
                              timeout_msg=Msg}=State)
  when Msg =/= undefined ->
    handle_info_result(dispatch_msg_noreply(Module, Msg, ModState),
                       clear_timeout_msg(State));
dispatch_info(Msg, #state{mod=Module, mod_state=ModState}=State) ->
    handle_info_result(dispatch_msg_noreply(Module, Msg, ModState), State).

handle_info_result({noreply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_info_result({reply, _Reply, ModState}, State) ->
    {noreply, set_mod_state(ModState, State)};
handle_info_result({noreply, ModState, {handle_msg, Msg}}, State) ->
    {noreply, set_timeout_msg(Msg, set_mod_state(ModState, State)), 0};
handle_info_result({noreply, ModState, {timeout, Timeout}}, State) ->
    {noreply, set_mod_state(ModState, State), Timeout};
handle_info_result({noreply, ModState, hibernate}, State) ->
    {noreply, set_mod_state(ModState, State), hibernate};
handle_info_result({reply, _Reply, ModState, {handle_msg, Msg}}, State) ->
    {noreply, set_timeout_msg(Msg, set_mod_state(ModState, State)), 0};
handle_info_result({reply, _Reply, ModState, {timeout, Timeout}}, State) ->
    {noreply, set_mod_state(ModState, State), Timeout};
handle_info_result({reply, _Reply, ModState, hibernate}, State) ->
    {noreply, set_mod_state(ModState, State), hibernate};
handle_info_result({stop, Reason}, State) ->
    {stop, Reason, State};
handle_info_result({stop, Reason, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_info_result({stop, Reason, _Reply, ModState}, State) ->
    {stop, Reason, set_mod_state(ModState, State)};
handle_info_result(Other, _State) ->
    exit({bad_return_value, Other}).

dispatch_terminate(Reason, #state{mod=Module, mod_state=ModState}) ->
    case erlang:function_exported(Module, terminate, 2) of
        true -> Module:terminate(Reason, ModState);
        false -> ok
    end.

dispatch_msg_noreply(Module, Msg, ModState) ->
    Module:handle_msg(Msg, noreply, ModState).

dispatch_msg(Module, Msg, From, ModState) ->
    Module:handle_msg(Msg, From, ModState).

set_mod_state(ModState, State) ->
    State#state{mod_state=ModState}.

set_timeout_msg(Msg, State) ->
    State#state{timeout_msg=Msg}.

clear_timeout_msg(State) ->
    State#state{timeout_msg=undefined}.
