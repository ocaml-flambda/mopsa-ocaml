=========================
Architecture of **Mopsa**
=========================

.. MOPSA developer manuel file

In order to start adding domains to the **Mopsa** static analyzer, we
first need to have a look at the architecture of the project.

Putting aside a lot of directories and files of no interest at this
point, the project is composed of 4 main directories : ``analyzer``,
``parsers``, ``share``, ``utils``.

``analyzer``
============

In addition to a directory ``analyzer/tests`` where tests will be stored
``analyzer`` contains two main directories :

-  ``analyzer/src/framework`` contains the engine of **Mopsa**, you
   should not have to modify any part of this directory while adding new
   abstract domains or language features. All the useful definitions in
   this directory are reassembled into a ``Mopsa`` module accessible
   anywhere in ``analyzer/src/lang``. More on that later.
-  ``analyzer/src/lang`` contains the definition of the abstract domains
   that will be the core components in the definition of a static
   analyzer. These abstract domains are separated by languages :
   ``analyzer/src/lang/universal``, ``analyzer/src/lang/c``,
   ``analyzer/src/lang/python``. This will be where abstract domains
   will be added during this tutorial.

``parsers``
===========

This directory contains the definition of the parsers used by **Mopsa**.

``share``
=========

This directory contains the configuration files used to describe an
analyzer as the composition of domains.

``utils``
=========

In this directory, you will find the definition of some libraries used
by **Mopsa**.

.. code:: text

   .
   ├── analyzer
   │   ├── lib
   │   ├── src
   │   │   ├── framework
   │   │   │   ├── combiners
   │   │   │   │   ├── domain
   │   │   │   │   │   └── optimized
   │   │   │   │   └── value
   │   │   │   ├── config
   │   │   │   │   └── abstraction
   │   │   │   ├── core
   │   │   │   │   └── ast
   │   │   │   ├── engines
   │   │   │   ├── hooks
   │   │   │   ├── lattices
   │   │   │   ├── output
   │   │   │   └── sig
   │   │   │       ├── abstraction
   │   │   │       ├── combiner
   │   │   │       └── reduction
   │   │   └── lang
   │   │       ├── c
   │   │       │   ├── common
   │   │       │   ├── cstubs
   │   │       │   ├── hooks
   │   │       │   ├── iterators
   │   │       │   ├── libs
   │   │       │   │   ├── clib
   │   │       │   │   │   ├── file_descriptor
   │   │       │   │   │   └── formatted_io
   │   │       │   │   └── libc
   │   │       │   │       ├── file_descriptor
   │   │       │   │       └── formatted_io
   │   │       │   ├── memory
   │   │       │   │   ├── lowlevel
   │   │       │   │   ├── packing
   │   │       │   │   │   └── reductions
   │   │       │   │   ├── reductions
   │   │       │   │   └── scalars
   │   │       │   │       └── pointers
   │   │       │   └── packing
   │   │       │       └── reductions
   │   │       ├── cfg
   │   │       │   └── iterators
   │   │       ├── python
   │   │       │   ├── data_model
   │   │       │   ├── desugar
   │   │       │   ├── flows
   │   │       │   ├── hooks
   │   │       │   ├── libs
   │   │       │   ├── objects
   │   │       │   └── types
   │   │       ├── repl
   │   │       ├── stubs
   │   │       │   └── iterators
   │   │       └── universal
   │   │           ├── heap
   │   │           ├── hooks
   │   │           ├── iterators
   │   │           │   └── interproc
   │   │           ├── numeric
   │   │           │   ├── reductions
   │   │           │   ├── relational
   │   │           │   └── values
   │   │           │       └── intervals
   │   │           ├── packing
   │   │           ├── partitioning
   │   │           └── strings
   │   └── tests
   │       ├── c
   │       │   ├── libc
   │       │   └── stubs
   │       ├── python
   │       │   └── types
   │       └── universal
   ├── parsers
   │   ├── c
   │   │   ├── lib
   │   │   └── src
   │   ├── c_stubs
   │   │   ├── lib
   │   │   └── src
   │   │       └── c_stubs_parser
   │   │           └── passes
   │   ├── python
   │   │   ├── lib
   │   │   └── src
   │   │       └── py_parser
   │   └── universal
   │       ├── lib
   │       └── src
   ├── share
   │   └── mopsa
   │       ├── configs
   │       │   ├── c
   │       │   ├── cfg
   │       │   ├── python
   │       │   └── universal
   │       └── stubs
   │           ├── c
   │           │   ├── libc
   │           │   │   ├── arpa
   │           │   │   ├── netinet
   │           │   │   └── sys
   │           │   └── mopsa
   │           └── python
   │               └── typeshed
   └── utils
       ├── lib
       ├── src
       │   ├── bitfields
       │   ├── congUtils
       │   └── itvUtils
       └── tests
