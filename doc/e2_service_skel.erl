-module(e2_service_skel).

-export([start_link/0, handle_msg/3]).

-record(state, {}).

start_link() ->
    e2_service:start_link(?MODULE, #state{}).

handle_msg(_Msg, _From, State) ->
    {noreply, State}.
