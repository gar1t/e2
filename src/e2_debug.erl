-module(e2_debug).

-export([trace_module/1,
         trace_module/2,
         trace_function/2,
         trace_function/3,
         trace_messages/1,
         trace_messages/2,
         stop_tracing/0]).

-define(OPTIONS_SCHEMA,
	[{file, [{default, undefined}, {type, string}]},
         {pattern, [{default, undefined}]},
         {no_return, [{default, false}, {type, boolean}]},
         {send_only, [{default, false}, {type, boolean}]},
         {receive_only, [{default, false}, {type, boolean}]},
         {process_events, [{default, false}, {type, boolean}]}]).

%%%===================================================================
%%% API
%%%===================================================================

trace_module(Module) ->
    trace_module(Module, []).

trace_module(Module, Options) ->
    Opts = e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    start_tracer(Opts),
    dbg_tpl(Module, Opts),
    dbg_calls().

trace_function(Module, Function) ->
    trace_function(Module, Function, []).

trace_function(Module, Function, Options) ->
    Opts = e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    start_tracer(Opts),
    dbg_tpl(Module, Function, Opts),
    dbg_calls().

trace_messages(Process) ->
    trace_messages(Process, []).

trace_messages(Process, Options) ->
    Opts = e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    start_tracer(Opts),
    dbg_messages(Process, Opts).

stop_tracing() ->
    dbg:stop_clear().

%%%===================================================================
%%% dbg wrappers
%%%===================================================================

dbg_tpl(Module, Opts) ->
    handle_dbg_tpl(dbg:tpl(Module, match_spec(Opts))).

dbg_tpl(Module, {Function, Arity}, Opts) ->
    handle_dbg_tpl(dbg:tpl(Module, Function, Arity, match_spec(Opts)));
dbg_tpl(Module, Function, Opts) ->
    handle_dbg_tpl(dbg:tpl(Module, Function, match_spec(Opts))).

handle_dbg_tpl({ok, _}) -> ok;
handle_dbg_tpl({error, Err}) -> error(Err).

dbg_calls() ->
    handle_dbg_p(dbg:p(all, c)).

handle_dbg_p({ok, _}) -> ok;
handle_dbg_p({error, Err}) -> error(Err).

dbg_messages(Process, Opts) ->
    handle_dbg_p(dbg:p(Process, trace_flags(Opts))).

trace_flags(Opts) ->
    message_flags(Opts, process_event_flags(Opts, [])).

message_flags(Opts, Acc) ->
    case e2_opt:value(send_only, Opts) of
        true -> [s|Acc];
        false ->
            case e2_opt:value(receive_only, Opts) of
                true -> [r|Acc];
                false -> [m|Acc]
            end
    end.

process_event_flags(Opts, Acc) ->
    case e2_opt:value(process_events, Opts) of
        true -> [p|Acc];
        false -> Acc
    end.

%%%===================================================================
%%% tracer
%%%===================================================================

start_tracer(Opts) ->
    start_dbg(),
    handle_tracer(dbg:tracer(process, tracer(Opts))).

tracer(Opts) ->
    Pattern = pattern_match_spec(e2_opt:value(pattern, Opts)),
    Out = trace_device(e2_opt:value(file, Opts)),
    {fun(Msg, []) -> maybe_trace(Msg, Pattern, Out), [] end, []}.

pattern_match_spec(undefined) -> undefined;
pattern_match_spec(Pattern) ->
    ets:match_spec_compile([{Pattern, [], ['$_']}]).

trace_device(undefined) -> standard_io;
trace_device(File) when is_list(File) ->
    handle_file_open(file:open(File, [append])).

handle_file_open({ok, Out}) -> Out;
handle_file_open({error, Err}) -> error({trace_file, Err}).

handle_tracer({ok, _Pid}) -> ok;
handle_tracer({error, already_started}) -> ok.

start_dbg() ->
    handle_dbg_start(catch(dbg:start())).

handle_dbg_start({ok, _Pid}) -> ok;
handle_dbg_start({'EXIT', {{case_clause, Pid}, _}})
  when is_pid(Pid) -> ok.

maybe_trace(Msg, undefined, Out) ->
    trace(Msg, Out);
maybe_trace({_, _, return_from, _, _}=Msg, _Pattern, Out) ->
    trace(Msg, Out);
maybe_trace(Msg, Pattern, Out) ->
    handle_pattern_match(apply_pattern(Pattern, Msg), Msg, Out).

apply_pattern(Pattern, Msg) ->
    ets:match_spec_run([msg_content(Msg)], Pattern).

msg_content({trace, _, call, {_, _, Args}}) -> Args;
msg_content({trace, _, return_from, _, Val}) -> Val;
msg_content({trace, _, send, Msg, _}) -> Msg;
msg_content({trace, _, 'receive', Msg}) -> Msg;
msg_content(Other) -> Other.

handle_pattern_match([_], Msg, Out) -> trace(Msg, Out);
handle_pattern_match([], _Msg, _Out) -> not_traced.

trace(Msg, Out) ->
    {Format, Data} = format_msg(Msg),
    io:format(Out, Format, Data).

format_msg({trace, Pid, call, {M, F, A}}) ->
    {"~n=TRACE CALL==== ~s ===~n~p -> ~s:~s/~p~n~p~n",
     [timestamp(), Pid, M, F, length(A), A]};
format_msg({trace, Pid, return_from, {M, F, Arity}, Val}) ->
    {"~n=TRACE RETURN==== ~s ===~n~p <- ~s:~s/~p~n~p~n",
     [timestamp(), Pid, M, F, Arity, Val]};
format_msg({trace, Pid, send, Msg, Dest}) ->
    {"~n=TRACE SEND==== ~s ===~n~p -> ~p~n~p~n",
     [timestamp(), Pid, Dest, Msg]};
format_msg({trace, Pid, 'receive', Msg}) ->
    {"~n=TRACE RECEIVE==== ~s ===~n~p~n~p~n", [timestamp(), Pid, Msg]};
format_msg(Other) ->
    HR = hr(),
    {"~s~nTRACE:~n~s~n  ~p~n~n", [HR, HR, Other]}.

timestamp() ->
    {{Y, M, D}, {H, Min, S}} = erlang:localtime(),
    io_lib:format("~p-~s-~p::~p:~p:~p", [D, month(M), Y, H, Min, S]).

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

hr() ->
    case io:columns() of
	{ok, N} -> binary:copy(<<"-">>, N - 2);
	{error, enotsup} -> binary:copy(<<"-">>, 78)
    end.

%%%===================================================================
%%% Match spec support
%%%===================================================================

match_spec(Opts) ->
    case e2_opt:value(no_return, Opts) of
	true ->
	    [{'_', [], []}];
	false ->
	    [{'_', [], [{return_trace}]}]
    end.
