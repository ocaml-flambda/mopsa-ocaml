===========================
*Stateful abstract domains*
===========================

.. MOPSA developer manuel

We have seen that an abstract domain provides transfer functions for the
handling of the constructions of the targeted language, and that The
analyzer obtained by composition of all the abstract domains chosen by
the user of **Mopsa** is itself an abstract domain. But abstract domains
need also carry information, this information is, in general, used as an
abstract representation of the memory. Some domains (as the one seen
before) do not carry any information (they are called *stateless
domains* or *iterators*) while others (*stateful domains*) carry some
information. In the same way that transfer functions are composed in a
more complex analyzer, the information stored by the domains are
assembled together forming the global state of the analyzer. The goal
being that this global state represents an abstraction of the concrete
semantics of the analyzed program.

In this section we focus on the definition of one of the simplest
stateful domain : the sign abstract domain.
