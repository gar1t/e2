-module(calc_server).

-behavior(e2_service).

-export([start_link/0, start_link/1]).

-export([init/1, handle_msg/3]).

-record(state, {socket}).

-define(TCP_OPTIONS, [binary, {active, false}, {reuseaddr, true}]).

-define(DEFAULT_PORT, 6666).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    start_link(app_env).

start_link(Options) ->
    e2_service:start_link(?MODULE, Options).

%%%-------------------------------------------------------------------
%%% Service callbacks
%%%-------------------------------------------------------------------

init(app_env) ->
    init(application:get_all_env());
init(Options) ->
    {ok, Socket} = gen_tcp:listen(listen_port(Options), ?TCP_OPTIONS),
    {ok, #state{socket=Socket}, {handle_msg, accept}}.

handle_msg(accept, noreply, #state{socket=LSocket}=State) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    {ok, _} = calc_handler_sup:start_handler(Socket),
    {noreply, State, {handle_msg, accept}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

listen_port(Options) ->
    proplists:get_value(port, Options, ?DEFAULT_PORT).
