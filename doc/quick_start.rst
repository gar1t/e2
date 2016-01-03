================
 e2 Quick Start
================

.. rubric:: Step 1 - Get e2

::

   $ git clone git://github.com/gar1t/e2.git

.. rubric:: Step 2 - Build e2

::

   $ cd e2
   $ make

.. rubric:: Step 3 - Create a test project

::

   $ make new-project appid=test appdir=~/e2-quick-start

.. rubric:: Step 4 - Create ``hello.erl``

::

   $ cd ~/e2-quick-start
   $ emacs src/hello.erl

Of course, you're free to use any editor in place of ``emacs`` :)

``~/e2-quick-start/src/hello.erl`` should look like this:

.. code-block:: erlang

   -module(hello).

   -behavior(e2_task).

   -export([start_link/0, handle_task/1]).

   start_link() ->
       e2_task:start_link(?MODULE, "Hello e2!~n").

   handle_task(Msg) ->
       e2_log:info(Msg),
       {repeat, Msg, 5000}.

.. rubric:: Step 5 - Register ``hello`` with your application

Modify ``~/e2-quick-start/src/test_app.erl`` to look like this:

.. code-block:: erlang

   -module(test_app).

   -behavior(e2_application).

   -export([init/0]).

   init() ->
       {ok, [hello]}.

.. rubric:: Step 6 - Run your application in the Erlang shell

Start the Erlang shell::

   $ make shell

In the Erlang shell::

   1> test:start().

You should see your ``hello`` task repeat every 5 seconds!

Stop the shell by typing ``CTRL-C`` twice (i.e. hold the control key down and
press ``C`` twice).

.. rubric:: Step 7 - Use the control scripts to run your app

To start your application::

   $ ./start

To check its status::

   $ ./status
   Application is running

To view its log::

   $ tail log/erlang.log.1

To stop it::

   $ ./stop

.. rubric:: Step 8 - Take a moment

- You just built and ran an `Erlang`_ application
- Erlang is different from any other language environment you've used before
- Erlang applications are *systems* of small, independent components
- The ``hello`` module is an example of a small, independent component
- You can build incredibly complex, robust systems using small, independent
  components!

.. rubric:: Next Steps

.. toctree::
   :maxdepth: 1

   tutorial

.. _Erlang: http://www.erlang.org
