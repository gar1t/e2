=================
Design Principles
=================

Embrace OTP
-----------

e2's primary focus is to make OTP easier to use -- not to change it.

e2 modules are relatively light weight wrappers around standard OTP modules.

Minimize Code, Maximize Correctness
-----------------------------------

Wherever possible, e2 will make sensible decisions for the user and provide the
ability to override those decisions.

Promote Useful Design Patterns
------------------------------

There are a number of design patterns lurking in OTP (e.g. services and
tasks). e2 should make these patterns obvious and easy to use.

Avoid Renaming Terminology
--------------------------

e2 should not attempt to rename standard nomenclature in Erlang/OTP. e2 names
should contribute to an improved understanding of OTP, not detract from it.

Keep It Small
-------------

e2 should never get big. "Big" is arbitrary, but you probably know it when you
see it.

Another way of putting it: e2 should only contain things that are really,
really useful and would be painful to create in user space.
