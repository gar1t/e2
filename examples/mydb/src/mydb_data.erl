-module(mydb_data).

-behavior(e2_service).

-export([start_link/1, get/1, put/2, del/1]).

-export([init/1, handle_msg/3]).

start_link(File) ->
    e2_service:start_link(?MODULE, File, [registered]).

get(Key) ->
    e2_service:call(?MODULE, {get, Key}).

put(Key, Value) ->
    e2_service:call(?MODULE, {put, Key, Value}).

del(Key) ->
    e2_service:call(?MODULE, {del, Key}).

init(File) ->
    {ok, open_db(File)}.

open_db(File) ->
    {ok, Db} = mydb_db:open(File),
    Db.

handle_msg({get, Key}, _From, Db) ->
    {reply, mydb_db:get(Db, Key), Db};
handle_msg({put, Key, Value}, _From, Db) ->
    {reply, mydb_db:put(Db, Key, Value), Db};
handle_msg({del, Key}, _From, Db) ->
    {reply, mydb_db:del(Db, Key), Db}.
