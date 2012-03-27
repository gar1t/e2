-module(calc_handler).

-behavior(e2_task).

-export([start_link/1]).

-export([handle_task/1, terminate/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link(Socket) ->
    e2_task:start_link(?MODULE, Socket).

%%%-------------------------------------------------------------------
%%% Task callback
%%%-------------------------------------------------------------------

handle_task(Socket) ->
    gen_tcp:send(Socket, <<"> ">>),
    case gen_tcp:recv(Socket, 0) of
        {ok, <<"q\r\n">>} ->
            {stop, normal};
        {ok, <<"quit\r\n">>} ->
            {stop, normal};
        {ok, Data} ->
            case calc:eval(Data) of
                {ok, Result} ->
                    gen_tcp:send(Socket, format_result(Result)),
                    gen_tcp:send(Socket, <<"\n">>);
                {error, Err} ->
                    gen_tcp:send(Socket, <<"ERROR: ">>),
                    gen_tcp:send(Socket, format_error(Err)),
                    gen_tcp:send(Socket, <<"\n">>)
            end,
            {repeat, Socket};
        {error, closed} ->
            {stop, normal};
        {error, Err} ->
            {stop, Err}
    end.

terminate(_Reason, Socket) ->
    gen_tcp:close(Socket).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

format_result(F) when is_float(F) ->
    iolist_to_binary(io_lib:format("~f", [F]));
format_result(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I)).

format_error(Err) ->
    try iolist_to_binary(Err) of
        Bin -> Bin
    catch
        error:badarg ->
            iolist_to_binary(io_lib:format("~p", [Err]))
    end.
