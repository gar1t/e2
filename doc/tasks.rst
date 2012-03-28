==========
 e2 Tasks
==========

A task is an e2 :doc:`service <services>` that primarily performs a single
thread of work.

A task can repeat if it needs to perform the work again.

Here's a simple task that looks up a value in a database and logs the result.

.. code-block:: erlang

   -module(db_read_task).

   -behavior(e2_task).

   -export([start_link/1, handle_task/1]).

   start_link(Db) ->
       e2_task:start_link(?MODULE, Db).

   handle_task(Db) ->
       Value = db:read_value(Db, "special_value"),
       e2_log:info({db_read_result, Value}),
       {stop, normal}.

Tasks are often implemented as "fire and forget" -- you start them and they
operated autonomously until they finish the work, or crash.

Tasks are typically started and supervised by :ref:`task supervisors
<task_supervisors>`. Task supervisors automatically remove tasks when they're
done.

Starting Tasks
==============

Start a task using ``e2_task:start_link/3``.

Initial State
=============

Like :doc:`e2_service <services>`, a task's state is initialized in one of
two ways:

- If the task exports ``init/1``, the result of ``init/1`` determines the task
  state (the second argument to ``e2_task:start_link/3`` is the argument to
  ``init/1``).
- If the task does not export ``init/1``, its initial state is the second
  argument used in the ``e2_task:start_link/3``.

Repeating Tasks
===============

Tasks can be configured to repeat. There are generally two scenarios where a
task is repeating:

- The task should run at regular intervals -- for example, a task that
  routinely checks for problems to fix
- The task should continually loop -- for example, a server that waits for new
  connections, dispatching them to handlers

Here's a task that scans a directory for invalid files every 30 seconds:

.. code-block:: erlang

   -module(dir_cleanup_task).

   -export([start_link/1, handle_task/1]).

   start_link(Dir) ->
       e2_task:start_link(?MODULE, Dir, [{repeat, 30000}]).

   handle_task(Dir) ->
       util:delete_files(util:find_invalid_files(Dir)),
       {repeat, Dir}.

This task will repeat forever. If wanted to stop at some point, it could return
``{stop, Reason}`` from ``handle_task/1``.

Delayed Tasks
=============

By default, ``handle_task/1`` is called as soon as the task process is started.
A task can alternatively be delayed.

Here's how a task can be started with an initial delay of 5 seconds:

.. code-block:: erlang

   e2_task:start_link(?MODULE, Args, [{delay, 5000}]).

Examples
========

The  `examples/tasks`_ project illustrates various task uses.

Here's a task that is used in `examples/calc`_ to handle client connections:

.. literalinclude:: ../examples/calc/src/calc_handler.erl
   :language: erlang

.. _examples/tasks: https://github.com/gar1t/e2/tree/master/examples/tasks
.. _examples/calc: https://github.com/gar1t/e2/tree/master/examples/calc


