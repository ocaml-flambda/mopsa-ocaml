.. _uni-options:

Universal Options
=================

Mopsa includes a Universal language that supports commonly used constructs (e.g., integers, loops, function calls) and provides ready-to-use abstractions and iterators.
These domains are commonly used in configurations for both C and Python.
We describe here the most important domains used in C and Python, as well as their command-line options.


Numeric Domains
---------------

In Universal, numeric values can be either mathematical (i.e., unbounded) integers or floating-point numbers (with proper rounding).
Mopsa proposes several non-relational and relational numeric abstractions:

Zero
~~~~

The simple domain ``universal.numeric.values.zero`` is a value abstraction that distinguishes between null and non-null integer values.

.. program:: universal.numeric.values.zero


Powerset
~~~~~~~~

The domain ``universal.numeric.values.powerset`` keeps a finite number of integer values per variable, which can be controlled with the option:

.. program:: universal.numeric.values.powerset

.. option:: -max-set-size <int>

   The maximal number of values in the set for a variable (default: ``10``).


.. _cong-options:

Congruences
~~~~~~~~~~~

The domain ``universal.numeric.values.congruences`` represents a set of integer values as a congruence :math:`a\mathbb{Z} + b`, where :math:`a \in \mathbb{N}` and :math:`b \in \mathbb{Z}`.

.. program:: universal.numeric.values.congruences

.. _itv-options:


Integer Intervals
~~~~~~~~~~~~~~~~~

The domain ``universal.numeric.values.intervals.integer`` uses (possibly unbounded) intervals to abstract integer values.

.. program:: universal.numeric.values.intervals.integer

.. _float_options:

Float Intervals
~~~~~~~~~~~~~~~

The domain ``universal.numeric.values.intervals.float`` uses intervals to abstract floating-point values.
Additionally, flags indicate the possible presence of special floating-point values (positive and negative infinities, not-a-number).
The abstraction does not distinguish between positive and negative zeros.
The domain has one option:

.. program:: universal.numeric.values.intervals.float

.. option:: -float-rounding-mode (near | zero | up | down | rnd)

   Set the IEEE rounding mode of floating-point computations (default: ``near``).

   ``rnd`` indicates that the rounding mode is not fixed and can change arbitrarily during program execution.
   It is useful to analyze soundly the program when the rounding mode is unknown.


.. _apron-options:

Relational Domains
~~~~~~~~~~~~~~~~~~

In addition to the previous non-relational abstractions, Mopsa provides a numeric relational domain ``universal.numeric.relation`` that is a wrapper around the `Apron library <https://antoinemine.github.io/Apron/doc/>`_.

.. program:: universal.numeric.relation

.. option:: -numeric (lineq | octagon | polyhedra)

   Select the relational numeric abstraction used by the domain: affine equalities, unit-two-variables-per-inequality (a.k.a. octagons), or affine inequalities (default: ``polyhedra``).


.. _loops-options:

Loops
-----

A generic iterator for ``while`` loops is implemented by the domain ``universal.iterators.loops``.
It supports unrolling and widening, which can be controlled with the options below.
As C and Python loops are translated into Universal ``while`` loops, they are also affected by these options.

.. program:: universal.iterators.loops

.. option:: -loop-decr-it

   Enable a single decreasing iteration after loop stabilisation.

.. option:: -loop-full-unrolling <bool>

   Unroll loops without applying widening (default: ``false``).

.. option:: -loop-full-unrolling-at [<file1>:]<line1>,...,[<filen>:]<linen>

   Fully unroll loops at specific program locations (default: empty).

.. option:: -loop-no-cache

   Disable the cache of previous loops fixpoints.

.. option:: -loop-unrolling <int>

   Set the number of unrolling iterations before joining the environments (default: ``1``).

.. option:: -loop-unrolling-at [<file1>:]<line1>:<unrolling1>,...,[<filen>:]<linen>:<unrollingn>

   Set the number of unrolling iterations at a specific program locations (default: empty).

.. option:: -widening-delay <int>

   Set the number of iterations using joins before applying a widening (default: ``0``).


.. _interproc-options:

Inter-procedural Analysis
-------------------------

Both the ``C`` and ``Python`` analyzers rely on common generic inter-procedural iterators for analyzing function calls:

Inlining
~~~~~~~~

The domain ``universal.iterators.interproc.inlining`` implements a precise iterator that inlines function calls.

.. program:: universal.iterators.interproc.inlining

Cache
~~~~~

The domain ``universal.iterators.interproc.sequential_cache`` improves the performance of inlining by caching the last calls and reuse them when possible.

.. program:: universal.iterators.interproc.sequential_cache

.. option:: -mod-interproc-size <int>

   Set the size of the cache (default: ``10``).

.. _recency:

Heap Recency
------------

Heap allocation can be abstracted by the domain ``universal.heap.recency`` that implements the recency abstraction algorithm [SAS06]_, that operates as follows:

1. Firstly, the user fixes an *allocation partitioning policy* that specifies how heap addresses are partitioned, e.g. by grouping all the allocations at a program location into one partition.

2. For each partition, the domain creates only two abstract addresses: one for the most recent allocation and another one for all previous allocations.

The domain also supports block deallocation and resizing (useful for C programs).
The recency abstraction is also used to model other kinds of dynamic resources, such as C files.
Such resources are managed in :ref:`stub contacts <c-contracts>`.


.. program:: Recency

.. option:: -default-alloc-pol (all | range | callstack | range_callstack)

   Select the allocation partitioning policy (default: ``range_callstack``).

   - ``all``                merge all allocations into a unique partition
   - ``range``              merge only allocations at the same program location
   - ``callstack``          merge only allocations on the same call stack
   - ``range_callstack``    merge only allocations at the same program location and call stack

.. program:: universal.heap.recency

.. option:: -hash-heap-address <bool>

   Display heap addresses as their hash (default: ``false``).

   By default, addresses are displayed with the full information about their partition, which can be large (e.g., if it includes a call stack).
   With this option, addresses are displayed as a hash of the partition, which makes the output more readable.


.. _unit-tests:

Unit Tests
----------

Mopsa has a special mode to perform unit tests.
In this mode, instead of analyzing the source from the top-level entry point (e.g., ``main`` for C), Mopsa will run a sequence of test functions and report the total number of successes and failures.
Every (global) C or Python function with a name starting with ``test_`` is considered to be a test and will be run.
Functional properties can be checked using language-specific assertion built-ins (such as ``_mopsa_assert`` in C, or ``mopsa.assert`` in Python).
The return code of the analysis is 0 if all tests passed, and 1 in case of a failure.

The Mopsa distribution includes some regression tests, which you can run with ``make tests``.
They are contained in :mopsa:`analyzer/tests/` and use this unit test mode.
An example unit test of C is described in :ref:`this section <c-unit-test>`.

.. program:: universal.iterators.unittest

.. option:: -unittest

   Activate unit test mode.

.. option:: -unittest-filter <f1>,<f2>,...,<fn>

   List of test functions to analyze (default: empty).

   If not specified, all the global functions starting with ``test_`` will be considered as test functions.


.. _uni-stub-options:

Contract-Based Stubs
--------------------

Stubs can actually be written in alternate languages, based on contracts, pre- and post-conditions, and logic formulas, instead of imperative statements.
For instance, there is a specific syntax for :ref:`C contracts <c-contracts>`.
However, a large part of the analysis of contracts is independent from the host language and implemented in Universal domains as a result.


.. program:: stubs.iterators.body

.. option:: -stub-ignore-case <function1>.<case1>,...,<functionn>.<casen>

   List of cases to ignore in specific stubs (default: empty).

   Stub contracts can feature ``case`` statements to indicate alternate behaviors for the function.
   Each case is labeled with a string.
   By default, analyzing a stub will join all the possible cases.
   With this option, it is possible to ignore certain cases, given by their label, of given function stubs.
   This is useful, for instance, to assume that library functions (such as ``malloc`` or ``open``) never fail (by ignoring, respectively, cases ``malloc.failure`` or ``open.failure``).

.. program:: stubs.iterators.fallback

.. option:: -stub-use-forall-loop-evaluation

   Use a fallback evaluation of universally quantified formulas, using loops.

.. [SAS06] Gogul Balakrishnan, Thomas W. Reps: Recency-Abstraction for Heap-Allocated Storage. SAS 2006: 221–239.
