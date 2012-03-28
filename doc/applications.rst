=================
 e2 Applications
=================

e2 applications are logical units of code that can be started and stopped
within an Erlang VM.

For more information on how Erlang uses *applications* to build complex
systems, see :doc:`erlang_os`.

An e2 application is implemented as a single ``e2_application`` behavior module
that defines the application's top-level processes. These processes are started
and supervised when the application is started.

Here's an ``e2_application`` behavior module that defined a single ``hello``
service:

.. code-block:: erlang

   -module(hello_app).

   -behavior(e2_application).

   -export([init/0]).

   init() ->
       {ok, [hello]}.

This is the application used in :doc:`quick_start` -- refer to that guide to
see how an application is used to automatically start services in an Erlang VM.

Application Supervisor
======================

The e2 application behavior works closely with ``e2_application_supervisor``,
which is responsible for supervising all of the top-level processes in your
application. The list of children returned by ``init/0`` (e.g. ``[hello]`` in
the example above) represent those processes that are stated and supervised
when the application is started.

.. note:: This section is under development. For the full specification of
   a child, see `e2_supervisor:start_link/3`_.

Default Supervisor Options
--------------------------

This ``init/0`` return value:

.. code-block:: erlang

    {ok, [hello]}

is equivalent to this:

.. code-block:: erlang

    {ok, [{{hello, start_link, []}, [{id, hello},
                                     {restart, permanent},
                                     {shutdown, brutal_kill}]}],
         [{strategy, one_for_one}, {max_restart, {1, 1}}]}

Child Spec Examples
-------------------

Start a ``hello`` service using ``hello:start_link()``:

.. code-block:: erlang

    {ok, [hello]}

Start the process using ``hello:start_link("Hello World!")``:

.. code-block:: erlang

    {ok, [{hello, start_link, ["Hello World!"]}]}

Give the process 1000 milliseconds to shutdown before killing it:

.. code-block:: erlang

    {ok, [{hello, [{shutdown, 1000}]}]}

Specify an argument to ``start_link`` and a shutdown interval:

.. code-block:: erlang

    {ok, [{{hello, start_link, ["Hello World!"]}, [{shutdown, 1000}]}]}

Start two services:

.. code-block:: erlang

   {ok, [hello, goodbye]}

Start two services with a different *max restart* setting for the supervisor:

.. code-block:: erlang

   {ok, [hello, goodbye], [{max_restart, {5, 5}}]}


.. _e2_supervisor\:start_link/3: api/e2_supervisor.html#start_link-3

