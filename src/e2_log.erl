-module(e2_log).

-export([info/1, info/2, error/1, error/2,
         tty/1,
         add_handler/1, add_handler/2, delete_handler/1]).

-define(is_string(S), is_list(S) orelse is_binary(S)).

info(Msg) when ?is_string(Msg) ->
    error_logger:info_msg(Msg);
info(Report) ->
    error_logger:info_report(Report).

info(Msg, Args) when ?is_string(Msg) ->
    error_logger:info_msg(Msg, Args);
info(Type, Report) ->
    error_logger:info_report(Type, Report).

error(Msg) when ?is_string(Msg) ->
    error_logger:error_msg(Msg);
error(Report) ->
    error_logger:error_report(Report).

error(Msg, Args) when ?is_string(Msg) ->
    error_logger:error_msg(Msg, Args);
error(Type, Report) ->
    error_logger:error_report(Type, Report).

tty(Flag) ->
    error_logger:tty(Flag).

add_handler(Handler) ->
    add_handler(Handler, []).

add_handler(Handler, Args) ->
    gen_event:add_handler(
      error_logger, {e2_log_handler, Handler}, {Handler, Args}).

delete_handler(Handler) ->
    gen_event:delete_handler(error_logger, {e2_log_handler, Handler}, []).
