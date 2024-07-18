.. MOPSA documentation main file, created by
   sphinx-quickstart on Mon Jan 20 13:12:34 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Overview
========

`Mopsa <https://gitlab.com/mopsa/mopsa-analyzer>`_ is an open-source multi-language static analyzer.
It aims at preventing software bugs at *compile time* before actually running the program.
Mopsa is a *sound* static analyzer that ensures that (1) all possible executions are covered and that (2) the analysis will terminate in finite time.
It is developed using the theory of `abstract interpretation <https://en.wikipedia.org/wiki/Abstract_interpretation>`_ that guarantees this soundness *by construction*.

This *User Manual* describes how to get started with the Mopsa analyzer.
It explains the primary options and tools provided by the analyzer that help in analyzing real-world programs.

   .. note::

      This software is a product of the `Mopsa research project <https://mopsa.lip6.fr/>`_.

If you use Mopsa and find it useful, we'd love to hear about it!
You can for instance post a `support request <https://gitlab.com/mopsa/mopsa-analyzer/-/issues/new?issuable_template=Support>`_  on the GitLab project to tell us about your experience.
In case of issue, you can also `file a bug report <https://gitlab.com/mopsa/mopsa-analyzer/-/issues/new?issuable_template=Bug>`_.


Scope
-----

Mopsa currently supports the analysis of a large subset of **C** and a subset of **Python 3**.
It reports *safety errors*:

- On C: run-time errors (overflows, invalid arithmetic or pointer operations, invalid memory accesses, double frees), assertion failures, failed preconditions for calls to the C library (invalid arguments, invalid formats, ...).

- On Python: uncaught exceptions, including types errors and assertion failures.

The precise list is given in the :ref:`reports section <reports>`.

.. _limitations:

Limitations
-----------

   .. warning::

      Mopsa is still an academic tool, under heavy development, and may contain **limitations** and **bugs**.

      Features, options, and output format are subject to change and may not be in sync with this documentation.

      Please report issues on the `issue tracker <https://gitlab.com/groups/mopsa/-/issues>`_ on the `GitLab project page <https://gitlab.com/mopsa/mopsa-analyzer>`_.


- On C: no support for recursive functions, concurrency, signals, bit-fields, long jumps, inline assembly, complex numbers, nor multidimensional variable length arrays; incomplete support for the C library; support for only a low-level, :ref:`cell-based memory model <cells-options>`.
- On Python: no support for ``eval``, ``async``, meta-classes, nor recursive functions; limited support for the standard library.


Mopsa aims at being sound, but it is not complete.
Soundness implies that (on a theoretical level) it reports all possible errors within the classes of safety errors it handles.
Incompleteness implies that it can also report spurious errors not occurring in any actual execution.
This is due to a loss of precision when computing program behaviors at an abstract level, and can sometimes be corrected by choosing more precise analysis options (as exemplified in this manual).

Mopsa is a `whole-program analysis`.
It requires the full source code of the analyzed program to be available, and an entry-point.
Alternatively, for library functions that are called by with no provided source, it is possible to provide instead :ref:`stubs <c-stubs>` modeling their effect.
Stubs are already provided for a significant part of the standard C library and a part of the Python standard library.


Additional resources
--------------------

- the source code of Mopsa is `available on GitLab <https://gitlab.com/mopsa/mopsa-analyzer>`_
- the `GitLab group <https://gitlab.com/mopsa>`_ has additional projects using Mopsa, notably analysis benchmarks
- the page for `the research project <https://mopsa.lip6.fr/>`_ has links to the scientific publications underlying the design of Mopsa

.. toctree::
   :hidden:
   :caption: Quick Start

   quick-start/installation
   quick-start/usage

.. toctree::
   :hidden:
   :caption: Outputs

   outputs/reports
   outputs/hooks

.. toctree::
   :hidden:
   :caption: Tools

   tools/mopsa-build
   tools/mopsa-db
   tools/mopsa-diff

.. toctree::
   :hidden:
   :caption: Analysis Options

   options/configurations
   options/general
   options/c
   options/python
   options/universal
   options/json-configurations

.. toctree::
   :hidden:
   :caption: Interactive Engine

   interactive/overview
   interactive/navigation
   interactive/inspection
   interactive/demo

.. toctree::
   :hidden:
   :caption: Pinpointing a Crash 

   reduction/general

.. toctree::
   :hidden:
   :caption: Built-ins and Stubs

   stubs/c
   stubs/c-builtins
   stubs/c-contracts
   stubs/python

.. toctree::
   :hidden:
   :caption: Benchmarks

   benchmarks/c
   benchmarks/python
   benchmarks/multilanguage

.. toctree::
   :caption: Index
   :hidden:

   genindex
