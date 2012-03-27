-module({{module}}).

-behavior(e2_service).

-export([start_link/0, ping/0]).

-export([init/1, handle_msg/3, terminate/2]).

-record(state, {}).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

ping() ->
    e2_service:call(?MODULE, ping).

%%%===================================================================
%%% Service callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_msg(ping, _From, State) ->
    {reply, pong, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
