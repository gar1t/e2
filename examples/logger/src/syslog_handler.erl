-module(syslog_handler).

-behavior(e2_log_handler).

-export([install/0]).

-export([init/1, handle_event/2]).

install() ->
    e2_log:add_handler(?MODULE).

init([]) ->
    ok = syslog:open("e2_logger_example", [], local0),
    {ok, []}.

handle_event(Event, State) ->
    log_event(Event),
    {ok, State}.

log_event({error_msg, Msg}) ->
    syslog:log(err, format_msg(Msg));
log_event({error_report, {std_error, Report}}) ->
    syslog:log(err, format_report(Report));
log_event({info_msg, Msg}) ->
    syslog:log(info, format_msg(Msg));
log_event({info_report, {std_info, Report}}) ->
    syslog:log(info, format_report(Report));
log_event(Other) ->
    io:format("**** NOT LOGGED: ~p~n", [Other]).

format_msg({Format, Data}) ->
    io_lib:format(Format, Data).

format_report(Report) ->
    io_lib:format("~p", [Report]).
