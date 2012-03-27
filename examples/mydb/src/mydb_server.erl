-module(mydb_server).

-behavior(e2_task).

-export([start_link/1]).

-export([init/1, handle_task/1]).

start_link(Port) ->
    e2_task:start_link(?MODULE, Port).

init(Port) ->
    {ok, listen(Port)}.

handle_task(Socket) ->
    dispatch_client(wait_for_client(Socket)),
    {repeat, Socket}.

listen(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [{reuseaddr, true}]),
    Socket.

wait_for_client(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    Client.

dispatch_client(Client) ->
    {ok, _} = mydb_client_handler_sup:start_handler(Client).
