-module(log_handler).

-export([install/0, init/1, handle_event/2]).

-behavior(e2_log_handler).

install() ->
    e2_log:add_handler(?MODULE, 1).

init(N) ->
    {ok, N}.

handle_event(Event, N) ->
    {Type, Msg} = format_event(Event),
    io:format(
      "[~s] ~b ~s ~s~n", [Type, N, iso_8601_fmt(erlang:localtime()), Msg]),
    {ok, N + 1}.

format_event({error_msg, {Format, Data}}) ->
    {"ERROR", io_lib:format(Format, Data)};
format_event({error_report, Report}) ->
    {"ERROR", io_lib:format("~p", [Report])};
format_event({warning_msg, {Format, Data}}) ->
    {"WARNING", io_lib:format(Format, Data)};
format_event({warning_report, Report}) ->
    {"WARNING", io_lib:format("~p", [Report])};
format_event({info_msg, {Format, Data}}) ->
    {"INFO", io_lib:format(Format, Data)};
format_event({info_report, Report}) ->
    {"INFO", io_lib:format("~p", [Report])}.

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format(
      "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B",
      [Year, Month, Day, Hour, Min, Sec]).
