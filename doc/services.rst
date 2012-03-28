=============
 e2 Services
=============

A service is an Erlang process that provides some functionality without an
application.

There are two general types of services:

- Registered services, which can be accessed by name
- Unregistered services, which must be accessed by their process ID

Example
=======

Here's a simple registered service that provides system wide access to a
database:

.. literalinclude:: db.erl
   :language: erlang

Let's break this module down step by step. The code for this module can be
divied into these categories:

- Module information
- Service initialization
- Service message handling
- Service termination

Module Information
------------------

All Erlang modules have this declaration -- it's the module name:

.. literalinclude:: db.erl
   :language: erlang
   :lines: 1

The ``behavior`` attribute tells us what type of module it is, and helps the
Erlang compiler check for any missing callback functions:

.. literalinclude:: db.erl
   :language: erlang
   :lines: 3

The ``exports`` attribute indicates which functions are callable from outside
the module:

.. literalinclude:: db.erl
   :language: erlang
   :lines: 5-7

Service Initialization
----------------------

Processes in Erlang are typically started with a ``start_link`` function. In
this case, we're starting our module using `e2_service:start_link/3`_:

.. literalinclude:: db.erl
   :language: erlang
   :lines: 9-10

The first argument is ``?MODULE`` -- a macro reference to this module
name, which is ``db``. This is used as the ``e2_service`` callback module.

The second argument is one of two things, depending on whether the callback
module exports ``init/1``:

- If the module exports ``init/1`` (as in this example), the second argument in
  ``start_link/3`` is the value used in the call to ``init/1``
- If the module does not export ``init/1``, the second argument is the initial
  process state

This module exports ``init/1``, which is called by e2 after the new process has
been created. This is where the service creates its initial *state*:

.. literalinclude:: db.erl
   :language: erlang
   :lines: 12-13

In this case, the service is connecting to a database, and returning the
database connection. This will be used in the first call to ``handle_msg/3``.

Process *state* is used by the service throughout its life type. Each time the
service responds to a message, it has an opportunity to perform some work and
modify its state.

Service Message Handling
------------------------

e2 services interact with clients by replying to messages sent by clients. In
this case, to get the system wide database, a client will send a message to the
server. This message sending is provided in an easy to use function, callable
by the client:

.. literalinclude:: db.erl
   :language: erlang
   :lines: 15-16

The function calls `e2_service:call/2`_, which sends the registered ``db``
process the ``get_db`` message. The call will wait until a result is sent back
by the service.

The server in turn handles the message:

.. literalinclude:: db.erl
   :language: erlang
   :lines: 18-19

This function is very simple -- it returns the system wide database ``Db`` that
was initialized in ``init/1``. The third element in the return value is the
*next state* of the server.

Service Termination
-------------------

When the service is stopped, it has a chance to cleanly close the database:

.. literalinclude:: db.erl
   :language: erlang
   :lines: 21-22

Using the Service
-----------------

If started is started, a client would use it this way:

.. code-block:: erlang

   Db = db:get(),
   some_db:set_value(Db, "some_key", "some_value").

This is a very simple service: it handles the initialization of a database
connection, makes it available to other Erlang processes, and handles the shut
down of the database.

It could be enhanced, if needed, with more features:

- Manage a pool of database connections
- Serialize access to a database connection

.. _e2_service\:start_link/3: api/e2_service.html#start_link-3
.. _e2_service\:call/2: api/e2_service.html#call-2

Example 2
=========

The following service is from `examples/utils`_ and illustrates how state is
managed and changes by a service:

.. literalinclude:: ../examples/utils/src/sequence.erl
   :language: erlang

.. _examples/utils: https://github.com/gar1t/e2/tree/master/examples/utils
