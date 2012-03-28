===============================
 Erlang as an Operating System
===============================

You can think of the Erlang VM as an operating system for your code.

Your code is organized into top level units called *applications*. Erlang
applications are similar to operating system applications:

* Can be started and stopped
* Can depend on other applications
* Errors in one application don't affect other applications
* Application communicate with one another through messages

Applications logically represent a set of processes that work together to
provide some set of services. Applications also provide static code that can be
used as modules.

As an example, consider the Apache web server `httpd`. `httpd` is an
*application*. You can install it, start it, use its services, and stop it when
you're done with it. You typically configure a server to start `httpd`
automatically when the server starts and it runs until the server is stopped.

While `httpd` is running, you can access its "services" (in this case HTTP and
HTTPS servers) through network sockets. When `httpd` is stopped, those sockets
are closed and the services are no longer available.

Apache also ships `httpd` with libraries that can be used by other programs --
e.g. `httpd` ships with a number of "mods". These libraries are not started as
OS processes, but they can be used by programs that dynamically link to them.

Erlang applications work the same way. In fact, Erlang has its own `httpd`
server -- a fully functional web server, complete with a set of "mods" similar
to Apache!

To illustrate, start an Erlang VM using ``erl``.

First, start the `inets` application, which provides a set of basic Internet
related client and server support::

   1> application:start(inets).
   ok

With the `inets` application started, we can create some servers. Let's start
an `httpd` server on port `8081` that uses `mod_get` to serve files from a
directory::

   2> inets:start(httpd, [{server_name, "hello"},
                          {port, 8081},
                          {server_root, "/tmp"},
                          {document_root, "/tmp"},
                          {modules, [mod_get]}]).
   {ok, <0.46.0>}

From a system shell, create a simple file in ``/tmp`` to read::

   $ echo "Hello, Erlang" > /tmp/hello.txt

In your browser, visit the URL `<http://localhost:8081/hello.txt>`_.

You should see "Hello, Erlang", which is being served by the Erlang httpd
server from ``/tmp``!

Now, stop the inets application::

   3> application:stop(inets).
   =INFO REPORT==== 14-Mar-2012::19:59:31 ===
       application: inets
       exited: stopped
       type: temporary

Try viewing the hello.txt URL again -- the server isn't running!

This might not seem terribly important -- but consider the implications of
"Erlang as an Operating System". Operating systems provide two invaluable
services to application developers:

* Manage complex systems of applications, consistently and reliably

* Let application developers build small, focused applications (e.g. web
  servers) that are decoupled yet communicate effectively using shared
  protocols

This is true of Erlang as well!

When you build Erlang "applications" you are, in fact, building Erlang
"systems". This is not merely a semantic difference! Systems are assemblies of
smaller, independent parts. With smaller, more focused, independent parts --
i.e. the *applications* you build with e2 -- you get a number of benefits:

* As a developer, you can use the principle `Separation of Concerns`_ to drive
  your development of services, features, etc

* With smaller, fine grained components, a fault is more limited in scope and
  impact than with larger, coarse grained components

* By defining the interactions between components as a set of protocols, you
  can change or replace implementations without breaking functionality

* You can deploy new functionality by installing and running new applications

.. _Separation of Concerns: http://en.wikipedia.org/wiki/Separation_of_concerns
