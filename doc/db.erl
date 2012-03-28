-module(db).

-behavior(e2_service).

-export([start_link/0, get/0]).

-export([init/1, handle_msg/3]).

start_link() ->
    e2_service:call(?MODULE, [], [registered]).

init([]) ->
    {ok, some_db:connect("some_user", "some_pwd")}.

get() ->
    e2_service:call(?MODULE, get_db).

handle_msg(get_db, _From, Db) ->
    {reply, Db, Db}.

terminate(Db) ->
    some_db:close(Db).
