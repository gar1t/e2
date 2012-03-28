%% ===================================================================
%% @author Garrett Smith <g@rre.tt>
%% @copyright 2011-2012 Garrett Smith
%%
%% @doc e2 task supervisor.
%%
%% See [http://e2project.org/supervisors.html e2 supervisor] documentation
%% for more information.
%%
%% @see e2_supervisor
%% @end
%% ===================================================================

-module(e2_task_supervisor).

-behaviour(supervisor).

-export([start_link/2, start_link/3, start_task/2]).

-export([init/1]).

-export([behaviour_info/1]).

%% @private
behaviour_info(callbacks) -> [].

-define(DEFAULT_MAX_RESTART, {1, 1}).
-define(DEFAULT_RESTART, temporary).

-define(OPTIONS_SCHEMA,
        [{max_restart,
          [{validate, fun validate_max_restart/1},
           {default, ?DEFAULT_MAX_RESTART}]},
         {registered, [{default, undefined}]}]).

-define(CHILD_OPTIONS_SCHEMA,
        [{restart,
          [{values, [permanent, temporary, transient]},
           {default, ?DEFAULT_RESTART},
           implicit]},
         {shutdown,
          [{validate, fun validate_shutdown/1},
           {default, brutal_kill}]}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts a linked task supervisor.
%% @equiv start_link(Module, ChildOrArgs, [])
%% @end
%%--------------------------------------------------------------------

start_link(Module, ChildOrArgs) ->
    start_link(Module, ChildOrArgs, []).

%%--------------------------------------------------------------------
%% @doc Starts a linked task supervisor.
%%
%% See {@link e2_supervisor:start_link/3} for details on Options.
%%
%% If Module exports `init/1', the second argument will be passed to
%% `init/1' when it's called. The return value from `init/1' is used
%% as the child spec.
%%
%% If Module does not export `init/1', the second argument is the child
%% spec. See {@link e2_supervisor:start_link/3} for details on ChildSpec.
%%
%% @spec start_link(Module, ChildOrArgs, Options) ->
%%                                      {ok, Pid} | {error, Reason}
%% Module = atom()
%% ChildOrArgs = ChildSpec | term()
%% @end
%%--------------------------------------------------------------------

start_link(Module, ChildOrArgs, Options) ->
    case exports_init(Module) of
        true ->
            start_supervisor_with_init(Module, ChildOrArgs, Options);
        false ->
            start_supervisor_with_child(Module, ChildOrArgs, Options)
    end.

%%--------------------------------------------------------------------
%% @doc Starts a new supervised task.
%%
%% ExtraArgs is a list of optional arguments to pass to the task's
%% start function (provided in the child spec when the supervisor was
%% started / initialized).
%%
%% @spec start_task(Supervisor, ExtraArgs) -> {ok, Pid} | {error, Reason}
%% Supervisor = pid() | atom()
%% ExtraArgs = [term()]
%% @end
%%--------------------------------------------------------------------

start_task(Sup, ExtraArgs) ->
    supervisor:start_child(Sup, ExtraArgs).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%% @private
init({Module, Args}) ->
    dispatch_init(Module, Args).

%%%===================================================================
%%% Internal functions
%%%===================================================================

exports_init(Module) ->
    erlang:function_exported(Module, init, 1).

start_supervisor_with_init(Module, Args, Options) ->
    e2_supervisor:start_link(
      ?MODULE, {Module, Args}, update_registered(Module, Options)).

start_supervisor_with_child(Module, Child, Options) ->
    e2_supervisor:start_link(Module, children(Child), sup_options(Options)).

validate_max_restart({MaxR, MaxT})
  when is_integer(MaxR), is_integer(MaxT),
       MaxR >= 0, MaxT >= 0 -> ok;
validate_max_restart(_) -> error.

validate_shutdown(Time) when is_integer(Time), Time >= 0 -> ok;
validate_shutdown(brutal_kill) -> ok;
validate_shutdown(_) -> error.

children({{M, F, A}, Options}) when is_atom(M), is_atom(F), is_list(A) ->
    Opts = e2_opt:validate(Options, ?CHILD_OPTIONS_SCHEMA),
    [{{M, F, A}, [{restart, e2_opt:value(restart, Opts)},
                  {shutdown, e2_opt:value(shutdown, Opts)}]}];
children({M, F, A}) ->
    children({{M, F, A}, []});
children({Mod, Options}) ->
    children({{Mod, start_link, []}, Options});
children(Mod) when is_atom(Mod) ->
    children({{Mod, start_link, []}, []});
children(Other) -> error({badarg, Other}).

update_registered(Module, Options) ->
    case proplists:get_value(registered, Options) of
        true -> [{registered, Module}|proplists:delete(registered, Options)];
        _ -> Options
    end.

sup_options(Options) ->
    e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    [simple_one_for_one|Options].

dispatch_init(Module, Args) ->
    handle_init_result(Module:init(Args)).

handle_init_result({ok, Child}) ->
    handle_init_result({ok, Child, []});
handle_init_result({ok, Child, Options}) ->
    {ok, children(Child), sup_options(Options)};
handle_init_result(ignore) -> ignore;
handle_init_result(Other) -> error({bad_return_value, Other}).
