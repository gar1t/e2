-module(e2_debug).

-export([trace_module/1,
         trace_function/2,
         trace_function/3,
         stop_all_tracing/0]).

%%%===================================================================
%%% API
%%%===================================================================

trace_module(Module) ->
    trace_module(Module, []).

trace_module(Module, Options) ->
    start_tracer(Options),
    dbg_tpl(Module, Options),
    dbg_p(Options).

trace_function(Module, Function) ->
    trace_function(Module, Function, []).

trace_function(Module, Function, Options) ->
    start_tracer(Options),
    dbg_tpl(Module, Function, Options),
    dbg_p(Options).

stop_all_tracing() ->
    dbg:stop_clear().

%%%===================================================================
%%% dbg wrappers
%%%===================================================================

dbg_p(_Options) ->
    handle_dbg_p(dbg:p(all, c)).

handle_dbg_p({ok, _}) -> ok;
handle_dbg_p({error, Err}) -> error(Err).

dbg_tpl(Module, Options) ->
    handle_dbg_tpl(dbg:tpl(Module, match_spec(Options))).

dbg_tpl(Module, {Function, Arity}, Options) ->
    handle_dbg_tpl(dbg:tpl(Module, Function, Arity, match_spec(Options)));
dbg_tpl(Module, Function, Options) ->
    handle_dbg_tpl(dbg:tpl(Module, Function, match_spec(Options))).

handle_dbg_tpl({ok, _}) -> ok;
handle_dbg_tpl({error, Err}) -> error(Err).

%%%===================================================================
%%% tracer
%%%===================================================================

start_tracer(_Options) ->
    start_dbg(),
    handle_tracer(dbg:tracer(process, tracer())).

tracer() ->
    {fun(Msg, []) -> trace(Msg), [] end, []}.

handle_tracer({ok, _Pid}) -> ok;
handle_tracer({error, already_started}) -> ok.

start_dbg() ->
    handle_dbg_start(catch(dbg:start())).

handle_dbg_start({ok, _Pid}) -> ok;
handle_dbg_start({'EXIT', {{case_clause, Pid}, _}})
  when is_pid(Pid) -> ok.

trace({trace, Pid, call, {M, F, A}}) ->
    io:format(
      "TRACE (~p) => ~s:~s/~b~n  ~p~n~n",
      [Pid, M, F, length(A), A]);
trace({trace, Pid, return_from, {M, F, Arity}, Val}) ->
    io:format(
      "TRACE (~p) <= ~s:~s/~b~n  ~p~n~n",
      [Pid, M, F, Arity, Val]);
trace(Other) ->
    io:format("TRACE:~n  ~p~n~n", [Other]).

%%%===================================================================
%%% Match spec support
%%%===================================================================

-define(MS_SCHEMA,
	[{no_return, [{default, false}, {type, boolean}]}]).

match_spec(Options) ->
    Opts = e2_opt:validate(Options, ?MS_SCHEMA),
    case e2_opt:value(no_return, Opts) of
	true ->
	    [{'_', [], []}];
	false ->
	    [{'_', [], [{return_trace}]}]
    end.
