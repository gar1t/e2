=============
 e2 Overview
=============

e2 is a light weight Erlang library that simplifies the process of writing
better software.

.. rubric:: Better Software

- Resilient in the face of unexpected faults
- Maintainable as it becomes more complex
- Performs acceptably

.. rubric:: e2 Features

- Applications are built as systems of federated services that act
  independently of one another
- Code is short and focused, reflecting the principle of `Separation of
  Concerns`_
- Applications can be debugged and fixed while running
- Applications continue to work, despite bugs and other faults

.. rubric:: e2 Benefits

- Problems are easier to solve because they can be made small, concrete and
  understandable
- Software enhancements and fixes can be deployed more often and in smaller
  units, reducing the overall risk of change
- Testing costs are lower due to Erlang's language design and fault tolerant
  architecture

.. rubric:: e2 vs. Other Productivity Tools

Software language and tool development tends to focus on two areas:

- Features that improve developer productivity
- Increasing program performance

Unfortunately, developer features and faster software often come at the expense
of software quality:

- Language features that hide complexity can also hide important design
  considerations that affect the way software runs and how it can be maintained

- Performance optimizations are often made without weighing the impact on other
  important software characteristics such as simplicity, reliability, and
  maintainability

e2 reflects Erlang's core design principle: performance and developer
productivity are important, but should not come at the expense of software
reliability and maintainability.

.. rubric:: Enough Talk!

.. toctree::
   :maxdepth: 1

   quick_start
   tutorial
   erlang

.. _Erlang: http://erlang.org
.. _Separation of Concerns: http://en.wikipedia.org/wiki/Separation_of_concerns
