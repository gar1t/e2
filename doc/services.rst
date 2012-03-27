=============
 e2 Services
=============

A service is an Erlang process that provides some functionality without an
application.

There are two general types of services:

- Registered services, which can be accessed by name
- Unregistered services, which must be accessed by their process ID

Here's a simple registered service that provides system wide access to a
database:

.. code-block:: erlang

   -module(db).

   -behavior(e2_service).

   -export([start_link/0, get/0]).

   -export([init/1, handle_msg/3]).

   start_link() ->
       e2_service:call(?MODULE, [], [registered]).

   get() ->
       e2_service:call(?MODULE, get_db).

   init([]) ->
       {ok, some_db:connect("some_user", "some_pwd")}.

   handle_msg(get_db, _From, Db) ->
       {reply, Db, Db}.

   terminate(Db) ->
       some_db:close(Db).

Provided this service is running, a client would use it this way:

.. code-block:: erlang

   Db = db:get(),
   some_db:set_value(Db, "some_key", "some_value").

So what?

- Services have a life cycle (init, run, shutdown) that clients don't need to
  worry about
- Fine grained services allow for separation of concerns
- The way to implement "global" functionality in an Erlang application
