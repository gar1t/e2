=======
 To Do
=======

This list is a work in progress. Please open tickets in github to request
changes or features. Or pull requests!

.. note:: There are a number of To Do items in `NOTES.org`_ that need to be
   added here (though many of the notes in that document have been implemented
   or no longer apply).

.. _NOTES.org: https://github.com/gar1t/e2/blob/master/NOTES.org

Documentation
=============

* Lots of general improvements all around!

* Annotate each project in "examples" - there's a lot that can be learned here.

* Missing any real documentation for e2_log and e2_log_handler (see
  `examples/logger`_ for some idea of how to use this).

* Missing docs for e2_opt. This is used throughout e2 however, so there are
  decent examples. `e2_opt_tests.erl`_ is also fairly complete set of examples.

* Also nothing on e2_publisher. See `examples/pubsub`_ for some examples.

* Examples on how to create custom e2_service behaviors. Note that both
  `e2_task.erl`_ and `e2_publisher.erl`_ use ``e2_service_impl`` so there's at
  least a little to to go on.

* Page that covers the big missing topics

  * Distributed Erlang support
  * Missing OTP behaviors (gen_event and gen_fsm)

* Add a data valiation feature to the tutorial (original plan)

.. _examples/logger: https://github.com/gar1t/e2/tree/master/examples/logger
.. _e2_opt_tests.erl: https://github.com/gar1t/e2/blob/master/test/e2_opt_tests.erl
.. _examples/pubsub: https://github.com/gar1t/e2/tree/master/examples/pubsub
.. _e2_task.erl: https://github.com/gar1t/e2/blob/master/test/e2_task.erl
.. _e2_publisher.erl: https://github.com/gar1t/e2/blob/master/test/e2_publisher.erl

Project and Module Templates
============================

* Various embedded web servers (Mochiweb, etc.)

* Module template for an e2_publisher

