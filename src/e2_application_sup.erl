%% ===================================================================
%% @author Garrett Smith <g@rre.tt>
%% @copyright 2011-2012 Garrett Smith
%%
%% @doc e2 application supervisor.
%%
%% An e2 application supervisor specializes in used for top-level of
%% an e2 applicaiton.
%%
%% This supervisor type is used by {@link e2_application} and is not
%% typically used directly.
%%
%% @see e2_application
%% @end
%% ===================================================================

-module(e2_application_sup).

-behavior(supervisor).

-export([start_link/1, start_link/2]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts a linked application supervisor process.
%% @equiv start_link(AppMod, [])
%% @end
%%--------------------------------------------------------------------

start_link(AppMod) ->
    supervisor:start_link({local, AppMod}, ?MODULE, [AppMod, []]).

%%--------------------------------------------------------------------
%% @doc Starts a linked application supervisor process.
%%
%% Refer to {@link e2_supervisor:start_link/3} for details about `Options'.
%%
%% @spec (AppMod, Options) -> {ok, Pid} | {error, Reason}
%% AppMod = atom()
%% @end
%%--------------------------------------------------------------------

start_link(AppMod, Options) ->
    supervisor:start_link({local, AppMod}, ?MODULE, [AppMod, Options]).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%% @private
init([AppMod, BaseOptions]) ->
    case AppMod:init() of
        {ok, Children} ->
            {ok, e2_supervisor:supervisor_spec(Children, BaseOptions)};
        {ok, Children, Options} ->
            {ok, e2_supervisor:supervisor_spec(
                   Children, Options ++ BaseOptions)};
        ignore ->
            ignore;
        Other ->
            exit({bad_return_value, Other})
    end.
