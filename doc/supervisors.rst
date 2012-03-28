================
 e2 Supervisors
================

Supervisors play an essential role in Erlang applications: they are responsible
for starting and monitoring processes.

All top-level processes in an Erlang application are managed by a
supervisor. In e2, this top level supervisor is provided automatically when you
use an :doc:`e2_application <applications>`.

.. note:: This section is under development. For examples of various supervisor
   types, see `examples/supervise`_.

.. _task_supervisors:

Task Supervisors
================

Task supervisors specialize in starting and monitoring :doc:`tasks
<tasks>`. Task supervisors automatically remove children when they're no longer
needed.

Here's an example of a task supervisor that starts a ``db_read_task`` (used as
the example in :doc:`tasks`):

.. code-block:: erlang

   -module(db_read_task_sup).

   -behavior(e2_task_supervisor).

   -export([start_link/0, start_reader/1]).

   start_link() ->
       e2_task_supervisor:start_link(?MODULE, db_read_task, [registered]).

   start_reader(Db) ->
       e2_task_supervisor:start_task(?MODULE, [Db]).

Note that this task supervisor specifies the ``registered`` option in
``start_link/0``. This causes the supervisor to be registered under the
``?MODULE`` name, which how it's references in ``start_reader/1``.

To ensure that this supervisor started when the app was started, you'd include
it in the application module child list. For example, modifying the example in
:doc:`applications`:

.. code-block:: erlang

   -module(hello_app).

   -behavior(e2_application).

   -export([init/0]).

   init() ->
       {ok, [hello, {db_read_task_sup, [supervisor]}]}.


Note that we added a ``supervisor`` list to the child spec -- this tells e2 to
register the specified module as a supervisor.

Once the supervisor is started, you can use it to kick off "db read" tasks like
this:

.. code-block:: erlang

   db_read_task_sup:start_reader(Db)

To understand why it's important to use supervisors to start processes in
Erlang, see the *Fault Tolerance* section in :doc:`tutorial`.

.. _examples/supervise: https://github.com/gar1t/e2/tree/master/examples/supervise
