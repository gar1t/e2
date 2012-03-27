%%% Support for custom service implementations.
%%%
%%% I'm not 100% happy with this name: hearkens to Java "Impl" classes
%%% and otherwise is totally obvious. Other options considered:
%%% e2_service_util (bad, overused "util" grab bag of misc supporting
%%% functions), e2_service_cb / e2_service_cbmod / e2_service_callback
%%% (arguably better as we typically refer to these modules as "callback
%%% modules, but it feels jargony).

-module(e2_service_impl).

-export([split_options/2, service_options/2,
         dispatch_init/2, init_result/2,
         dispatch_handle_msg/4, handle_msg_result/2,
         dispatch_terminate/3, set_trap_exit/1]).

%%%===================================================================
%%% API
%%%===================================================================

split_options(Module, Options) ->
    split_options(Module, Options, [], []).

service_options(Module, Options) ->
    {ServiceOpts, OtherOpts} = split_options(Module, Options),
    ServiceOpts ++ OtherOpts.

dispatch_init(Module, Args) ->
    case erlang:function_exported(Module, init, 1) of
        true ->
            handle_dispatch_init(Module:init(Args));
        false ->
            handle_dispatch_init({ok, Args})
    end.

init_result({ok, _}, State) ->
    {ok, State};
init_result({ok, _, Timeout}, State) when is_integer(Timeout) ->
    {ok, State, Timeout};
init_result({ok, _, FirstMsg}, State) ->
    {ok, State, FirstMsg};
init_result({stop, Reason}, _) ->
    {stop, Reason};
init_result(ignore, _) ->
    ignore.

dispatch_handle_msg(Module, Msg, From, State) ->
    handle_dispatch_handle_msg(Module:handle_msg(Msg, From, State)).

handle_msg_result({noreply, _}, State) ->
    {noreply, State};
handle_msg_result({noreply, _, Timeout}, State) ->
    {noreply, State, Timeout};
handle_msg_result({reply, Reply, _}, State) ->
    {reply, Reply, State};
handle_msg_result({reply, Reply, _, Timeout}, State) ->
    {reply, Reply, State, Timeout};
handle_msg_result({stop, Reason, _}, State) ->
    {stop, Reason, State}.

dispatch_terminate(Module, Reason, State) ->
    case erlang:function_exported(Module, terminate, 2) of
        true -> Module:terminate(Reason, State);
        false -> ok
    end.

set_trap_exit(Module) ->
    TerminateExported = erlang:function_exported(Module, terminate, 2),
    process_flag(trap_exit, TerminateExported).

%%%===================================================================
%%% Internal functions
%%%===================================================================

split_options(_Module, [], ServiceOpts, ImplOpts) ->
    {ServiceOpts, ImplOpts};
split_options(Module, [registered|Rest], ServiceOpts, ImplOpts) ->
    split_options(Module, Rest, [{registered, Module}|ServiceOpts], ImplOpts);
split_options(Module, [{registered, Name}|Rest], ServiceOpts, ImplOpts) ->
    split_options(Module, Rest, [{registered, Name}|ServiceOpts], ImplOpts);
split_options(Module, [O|Rest], ServiceOpts, ImplOpts) ->
    split_options(Module, Rest, ServiceOpts, [O|ImplOpts]).

handle_dispatch_init({ok, State}) ->
    {{ok, State}, State};
handle_dispatch_init({ok, State, Timeout}) when is_integer(Timeout) ->
    {{ok, State, Timeout}, State};
handle_dispatch_init({ok, State, FirstMsg}) ->
    {{ok, State, FirstMsg}, State};
handle_dispatch_init({stop, Reason}) ->
    {{stop, Reason}, undefined};
handle_dispatch_init(ignore) ->
    {ignore, undefined};
handle_dispatch_init(Other) ->
    exit({bad_return_value, Other}).

handle_dispatch_handle_msg({noreply, State}) ->
    {{noreply, State}, State};
handle_dispatch_handle_msg({noreply, State, Timeout}) ->
    {{noreply, State, Timeout}, State};
handle_dispatch_handle_msg({reply, Reply, State}) ->
    {{reply, Reply, State}, State};
handle_dispatch_handle_msg({reply, Reply, State, Timeout}) ->
    {{reply, Reply, Timeout}, State};
handle_dispatch_handle_msg({stop, Reason, State}) ->
    {{stop, Reason, State}, State};
handle_dispatch_handle_msg(Other) ->
    exit({bad_result_value, Other}).
