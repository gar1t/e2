-module(sample_process).

-behavior(e2_task).

-export([start_link/0, init/1, handle_task/1]).

-define(DEFAULT_COUNT, 5).
-define(REPEAT_DELAY, 1000).

start_link() ->
    e2_task:start_link(?MODULE, []).

init([]) ->
    {ok, {1, log_count()}}.

log_count() ->
    case application:get_env(sample_log_count) of
        {ok, Count} -> Count;
        undefined -> ?DEFAULT_COUNT
    end. 

handle_task({N, Max}) when N > Max ->
    {stop, normal};
handle_task({N, Max}) ->
    e2_log:info({sample_process_log_event, N}),
    {repeat, {N + 1, Max}, ?REPEAT_DELAY}.
