%% ===================================================================
%% @author Garrett Smith <g@rre.tt>
%% @copyright 2011-2012 Garrett Smith
%%
%% @doc e2 supervisor.
%%
%% An e2 supervisor provides identical functionality to that of an
%% [http://www.erlang.org/doc/man/supervisor.html OTP supervisor] but
%% uses a simpler interface.
%%
%% See [http://e2project.org/supervisors.html e2 supervisor] documentation
%% for more information.
%% @end
%% ===================================================================

-module(e2_supervisor).

-behaviour(supervisor).

-export([start_link/2, start_link/3, supervisor_spec/2]).

-export([init/1]).

-export([behaviour_info/1]).

%% @private
behaviour_info(callbacks) -> [].

-define(DEFAULT_STRATEGY, one_for_one).
-define(DEFAULT_MAX_RESTART, {1, 1}).
-define(DEFAULT_RESTART, permanent).
-define(DEFAULT_SHUTDOWN, brutal_kill).

-define(STRATEGIES,
        [one_for_all,
         one_for_one,
         rest_for_one,
         simple_one_for_one]).

-define(OPTIONS_SCHEMA,
        [{strategy,
          [{values, ?STRATEGIES}, implicit,
           {default, ?DEFAULT_STRATEGY}]},
         {max_restart,
          [{validate, fun validate_max_restart/1},
           {default, ?DEFAULT_MAX_RESTART}]},
         {registered, [{default, undefined}]}]).

-define(CHILD_OPTIONS_SCHEMA,
        [{id, [optional]},
         {type,
          [{values, [worker, supervisor]},
           {default, worker},
           implicit]},
         {restart,
          [{values, [permanent, temporary, transient]},
           {default, ?DEFAULT_RESTART},
           implicit]},
         {shutdown,
          [{validate, fun validate_shutdown/1},
           {default, ?DEFAULT_SHUTDOWN}]}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts a linked e2 supervisor.
%% @equiv start_link(Module, ChildrenOrArgs, [])
%% @end
%%--------------------------------------------------------------------

start_link(Module, ChildrenOrArgs) ->
    start_link(Module, ChildrenOrArgs, []).

%%--------------------------------------------------------------------
%% @doc Starts a linked e2 supervisor.
%%
%% The e2 supervisor API lets you create child specs with minimal
%% complexity, picking sensible defaults for missing values.
%%
%% If `Module' exports `init/1', `ChildrenOrArgs' will be used as the
%% argument to that function and the result of that function will be used
%% as the child spec.
%%
%% <h4>Defaults</h4>
%% <pre>
%% Function = start_link
%% Args = []
%% ChildOptions = [{id, Module}, {restart, permanent}, {shutdown, brutal_kill}]
%% Options = [{strategy, one_for_one}, {max_restart, {1, 1}}]
%% </pre>
%%
%% Refer to [http://e2project.org/supervisors.html e2 supervisor] and
%% [http://e2project.org/applications.html e2 applications] documentation
%% for more information.
%%
%% @spec (Module, ChildrenOrArgs, Options) -> {ok, Pid} | {error, Reason}
%% Module = module()
%% ChildrenOrArgs = Children | term()
%% Children = [ChildSpec]
%% ChildSpec = {{Module, Function, Args}, ChildOptions}
%%           | {Module, Function, Args}
%%           | {Module, ChildOptions}
%%           | Module
%% Module = atom()
%% Function = atom()
%% Args = [term()]
%% ChildOptions = [ChildOption]
%% ChildOption = {id, term()}
%%             | {restart, Restart}
%%             | {shutdown, Shutdown}
%% Restart = permanent | temporary | transient
%% Shutdown = brutal_kill | integer()
%% Options = [Option]
%% Option = {strategy, Strategy}
%%        | {max_restart, {Max, Seconds}}
%%        | Registered
%% Strategy = one_for_all | one_for_one | rest_for_one | simple_one_for_one
%% Max = integer()
%% Seconds = integer()
%% Registered = registered | {registered, Name}
%% Name = atom()
%% @end
%%--------------------------------------------------------------------

start_link(Module, ChildrenOrArgs, Options) ->
    case exports_init(Module) of
        true ->
            start_supervisor_with_init(Module, ChildrenOrArgs, Options);
        false ->
            start_supervisor_with_spec(Module, ChildrenOrArgs, Options)
    end.

%%--------------------------------------------------------------------
%% @doc Returns an OTP supervisor spec for a list of child specs and
%% e2 supervisor options.
%%
%% See {@link start_link/3} for details on `ChildSpec' and `Options'.
%%
%% @spec (Children, Options) -> OTPSupervisorSpec
%% @end
%%--------------------------------------------------------------------

supervisor_spec(Children, Options) ->
    ValidatedOpts = e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    {restart_spec(ValidatedOpts), child_specs(Children)}.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%% @private
init({init, Module, Args}) ->
    dispatch_init(Module, Args);
init({spec, Spec}) ->
    {ok, Spec}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

exports_init(Module) ->
    erlang:function_exported(Module, init, 1).

start_supervisor_with_init(Module, Args, Options) ->
    ValidatedOpts = e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    start_supervisor(sup_name(Module, ValidatedOpts), {init, Module, Args}).

start_supervisor_with_spec(Module, Children, Options) ->
    ValidatedOpts = e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    Spec = {restart_spec(ValidatedOpts), child_specs(Children)},
    start_supervisor(sup_name(Module, ValidatedOpts), {spec, Spec}).

sup_name(Module, Options) ->
    case e2_opt:value(registered, Options) of
        undefined -> unregistered;
        true -> {registered, Module};
        Name when is_atom(Name) -> {registered, Name};
        Other -> error({badarg, Other})
    end.

start_supervisor(unregistered, Args) ->
    supervisor:start_link(?MODULE, Args);
start_supervisor({registered, Name}, Args) ->
    supervisor:start_link({local, Name}, ?MODULE, Args).

validate_max_restart({MaxR, MaxT})
  when is_integer(MaxR), is_integer(MaxT),
       MaxR >= 0, MaxT >= 0 -> ok;
validate_max_restart(_) -> error.

validate_shutdown(Time) when is_integer(Time), Time >= 0 -> ok;
validate_shutdown(brutal_kill) -> ok;
validate_shutdown(_) -> error.

child_specs(Children) ->
    lists:map(fun child_spec/1, Children).

child_spec({{M, F, A}, Options}) when is_atom(M), is_atom(F), is_list(A) ->
    Opts = e2_opt:validate(Options, ?CHILD_OPTIONS_SCHEMA),
    Id = e2_opt:value(id, Opts, M),
    Restart = e2_opt:value(restart, Opts),
    Shutdown = e2_opt:value(shutdown, Opts),
    Type = e2_opt:value(type, Opts),
    {Id, {M, F, A}, Restart, Shutdown, Type, [M]};
child_spec({M, F, A}) when is_atom(M), is_atom(F), is_list(A) ->
    child_spec({{M, F, A}, []});
child_spec({Mod, Options}) when is_atom(Mod), is_list(Options) ->
    child_spec({{Mod, start_link, []}, Options});
child_spec(Mod) when is_atom(Mod) ->
    child_spec({{Mod, start_link, []}, []});
child_spec(Other) -> error({badarg, Other}).

restart_spec(Opts) ->
    {MaxR, MaxT} = e2_opt:value(max_restart, Opts),
    {e2_opt:value(strategy, Opts), MaxR, MaxT}.

dispatch_init(Module, Args) ->
    handle_init_result(Module:init(Args)).

handle_init_result({ok, Children}) ->
    handle_init_result({ok, Children, []});
handle_init_result({ok, Children, Options}) ->
    ValidatedOpts = e2_opt:validate(Options, ?OPTIONS_SCHEMA),
    {ok, {restart_spec(ValidatedOpts), child_specs(Children)}};
handle_init_result(ignore) -> ignore;
handle_init_result(Other) -> error({bad_return_value, Other}).
