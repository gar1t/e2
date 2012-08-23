=====
Locks
=====

``e2_lock`` provides a simple locking facility that can be used to coodinate
activities processes that don't have knowledge of one another.

Locks are useful for guarding against simultaneous use of external resources
such as files or network sockets.

Here's a simple locking service that can be used to guard against simultaneous
access of a file:

.. code-block:: erlang

   -module(file_lock).

   -export([start_link/0, acquire/1, release/1]).

   start_link() ->
       e2_lock:start_link(?MODULE).

   acquire(File) ->
       e2_lock:acquire(?MODULE, File).

   release(Lock) ->
       e2_lock:release(?MODULE, Lock).

And here's how the service could be used:

.. code-block:: erlang

   write_hello(File) ->
       {ok, Lock} = file_lock:acquire(File),
       ok = file:write_file(File, <<"Hello">>),
       file_lock:release(Lock).

   write_goodbye(File) ->
       {ok, Lock} = file_lock:acquire(File),
       ok = file:write_file(File, <<"Goodbye">>),
       file_lock:release(Lock).

Lock Behavior
=============

Locks are in effect until released or the calling process exits.

Locks are qualified by references so that each call to ``acquire`` results in a
unique lock.

If a lock has alreay been acquired, subsequent calls to ``acquire`` will queue
the caller and will block until the lock becomes available. Callers wait in
FIFO order, each receiving the lock as the previous callers release it.

Locks must be released using the lock returned by ``acquire``. However a lock
may be forcibly released using just their name via ``force_release``.

Future Enhancements
===================

* Asynchronous acquire and release notification
* Lock time-to-live
* Acquire timeouts
