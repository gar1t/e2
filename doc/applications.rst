=================
 e2 Applications
=================

e2 applications are logical units of code that can be started and stopped
within an Erlang VM.

An e2 application is implemented as a single `e2_application` behavior module
that defines the application's top-level processes. These processes are started
and supervised when the application is started.

Here's an `e2_application` behavior module that defined a single `hello`
service:

.. code-block:: erlang

   -module(hello_app).

   -behavior(e2_application).

   -export([init/0]).

   init() ->
       {ok, [hello]}.
