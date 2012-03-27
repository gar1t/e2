=========
 Facades
=========

Facades are Erlang modules that define a simplified interface to the
functionality provided by an :doc:`application <applications>`.

Facades should always the same name as the Erlang application.

Facades often provide a convenience function ``start/0`` that can be used to
start the application along with its dependencies. You can use
``e2_application:start_with_dependencies/1`` to do this.

Here's a minimal facade example:

.. code-block:: erlang

   -module(myapp).

   -export([start/0, stop/0])

   start() ->
       e2_application:start_with_dependencies(myapp).

   stop() ->
       application:stop(myapp).

Projects created with the ``new-project`` e2 make target
