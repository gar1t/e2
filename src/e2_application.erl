%% ===================================================================
%% @author Garrett Smith <g@rre.tt>
%% @copyright 2011-2012 Garrett Smith
%%
%% @doc e2 application behavior.
%%
%% e2 applications combine OTP applications and the top level supervisor
%% into one construct, though each is implemented as a separate module.
%%
%% Refer to [http://e2project.org/applications.html e2 application]
%% documentation for more details.
%%
%% @see e2_application_sup
%% @end
%% ===================================================================

-module(e2_application).

-behavior(application).

-export([start/2, stop/1, start_dependencies/1, start_with_dependencies/1]).

-export([behaviour_info/1]).

%% @private
behaviour_info(callbacks) -> [{init, 0}].

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Start an application's dependencies.
%% @spec (App) -> ok | {error, Reason}
%% App = atom()
%% @end
%%--------------------------------------------------------------------

start_dependencies(App) ->
    start_apps(get_dependencies(App)).

%%--------------------------------------------------------------------
%% @doc Start an application along with its dependencies.
%% @spec (App) -> ok | {error, Reason}
%% App = atom()
%% @end
%%--------------------------------------------------------------------

start_with_dependencies(App) ->
    start_dependencies(App),
    application:start(App).

%%%===================================================================
%%% application callbacks
%%%===================================================================

%% @private
start(normal, [E2App]) when is_atom(E2App) ->
    e2_application_sup:start_link(E2App).

%% @private
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_dependencies(App) ->
    lists:reverse(get_dependencies([App], [], [])).

get_dependencies([], _Parents, Deps) ->
    Deps;
get_dependencies([App|Rest], Parents, Deps) ->
    case in_deps(App, Deps) of
	true -> get_dependencies(Rest, Parents, Deps);
	false ->
	    case is_cycle(App, Parents) of
		true -> erlang:error({cycle, [App|Parents]});
		false -> ok
	    end,
	    case load_app(App) of
		ok -> ok;
		LoadError -> erlang:error(LoadError)
	    end,
	    DepDeps = get_dependencies(required_apps(App),
				       [App|Parents], Deps),
	    case Parents of
		%% Using parents as a proxy to determine if we're dealing with
		%% the root app. If so (parents empty), we don't want to
		%% include the app in the dependency list.
		[] ->
		    get_dependencies(Rest, Parents, DepDeps);
		_ ->
		    get_dependencies(Rest, Parents, [App|DepDeps])
	    end
    end.

in_deps(_App, []) ->
    false;
in_deps(App, [App|_Rest]) ->
    true;
in_deps(App, [_|Rest]) ->
    in_deps(App, Rest).

is_cycle(_App, []) ->
    false;
is_cycle(App, [Parent|Rest]) ->
    case App == Parent of
	true -> true;
	false -> is_cycle(App, Rest)
    end.

required_apps(App) ->
    {ok, Keys} = application:get_all_key(App),
    proplists:get_value(applications, Keys, []).

load_app(App) ->
    case application:load(App) of
	ok -> ok;
	{error, {already_loaded, App}} -> ok;
	Other -> Other
    end.

-define(is_core(App), App == kernel orelse App == stdlib).

start_apps([]) ->
    ok;
start_apps([App|Rest]) when ?is_core(App) ->
    start_apps(Rest);
start_apps([App|Rest]) ->
    case application:start(App) of
	ok ->
            error_logger:info_msg("Started ~p~n", [App]);
	{error, {already_started, App}} ->
            ok;
	Other ->
            timer:sleep(100), % Ugh. Hack to let sasl print errors.
            erlang:error(Other)
    end,
    start_apps(Rest).
