-module(e2_application_sup).

-behavior(supervisor).

-export([start_link/1, start_link/2]).

-export([init/1]).

start_link(AppMod) ->
    supervisor:start_link({local, AppMod}, ?MODULE, [AppMod, []]).

start_link(AppMod, Options) ->
    supervisor:start_link({local, AppMod}, ?MODULE, [AppMod, Options]).

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
