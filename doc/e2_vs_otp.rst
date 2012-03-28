=========
e2 vs OTP
=========

e2_service vs gen_server
========================

`gen_server`_ is the workhorse OTP behavior -- it's the primary OTP interface
to Erlang processes.

:doc:`e2_service <services>` is the equivalent in e2.

``e2_service`` is identical to ``gen_server`` under the covers -- it is a
``gen_server`` behavior. The additional overhead in using an e2 service is the
cost of on additional functional call for each ``gen_server`` operation.

``e2_service`` differes from ``gen_server`` as follows:

- ``init/1`` and ``terminate/2`` are optional in e2
- ``handle_call/3``, ``handle_cast/2``, and ``handle_info/2`` are consolidated
  into a single ``handle_msg/3`` callback in e2
- ``e2_sevice`` does not support ``code_change/3`` [#code_change]_

Here's the minimum foot print of ``gen_server``:

.. literalinclude:: gen_server_skel.erl
   :language: erlang

Here's the minimum foot print of ``e2_service``:

.. literalinclude:: e2_service_skel.erl
   :language: erlang

e2_application vs application
=============================

The `application`_ OTP behavior is represented by the :doc:`e2_application
<applications>` behavior.

``e2_application`` implicitly uses a top-level supervisor rather than require a
separate module. e2 application modules provide the top level list of
supervised child specs.

In OTP, applications typically start the top level supervisor.

e2_task vs gen_server
=====================

An :doc:`e2 task <tasks>` is a type of sevice that runs active after it's
started. This is a cleanup of the following pattern used in ``gen_server``
behaviors:

.. code-block:: erlang

   start_link() ->
       gen_server:start_link({local, ?MODULE}, []).

   init(Args) ->
       {ok, init_state(Args), 0}.

   handle_info(timeout, State) ->
       %% TODO: start my task here
       {stop, normal, State}.

Returning 0 for the timeout value in ``init/1`` will cause ``gen_server`` to
call ``handle_info/2`` with a ``timeout`` message before processing any
messages sent to the process from external sources. This allows a "task" to run
immediately after ``init/1`` returns.

In e2, this is handled with an explicit task behavior:

.. code-block:: erlang

   start_link() ->
       e2_task:start_link(?MODULE, []).

   init(Args) ->
       {ok, init_state(Args)}.

   handle_task(State) ->
       %% TODO: start my task here
       {stop, normal, State}.

e2_task_supervisor vs supervisor
================================

Task supervisors in e2 are ``simple_one_for_one`` supervisors. This is the one
supervisor type that automatically removes it children when they're completed
-- something that's needed for typical "task" applications.

.. rubric:: Footnotes

.. [#code_change] This will likely be exposed in the future.

.. _gen_server: http://www.erlang.org/doc/man/gen_event.html
.. _application: http://www.erlang.org/doc/man/application.html

