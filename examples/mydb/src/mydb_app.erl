-module(mydb_app).

-behavior(e2_application).

-export([init/0]).

-define(DEFAULT_DB_FILE, "/var/lib/mydb/data.db").
-define(DEFAULT_PORT, 1234).

init() ->
    {ok, [{mydb_data, start_link, [db_file()]},
          {mydb_client_handler_sup, [supervisor]},
          {mydb_server, start_link, [server_port()]}
         ]}.

db_file() ->
    app_config(db_file, ?DEFAULT_DB_FILE).

server_port() ->
    app_config(server_port, ?DEFAULT_PORT).

app_config(Name, Default) ->
    handle_app_env(application:get_env(Name), Default).

handle_app_env({ok, Value}, _Default) -> Value;
handle_app_env(undefined, Default) -> Default.
