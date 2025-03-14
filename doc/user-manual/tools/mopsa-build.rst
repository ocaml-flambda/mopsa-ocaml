.. _mopsa-build:

``mopsa-build``
===============


Analyzing Complex C Projects
----------------------------

Mopsa provides an easy workflow to analyze C programs that use a build system, like ``make``.
The workflow is designed to be as little intrusive as possible (as long as you are able to compile the program to analyze).
It uses a simple wrapper tool, ``mopsa-build``, provided with Mopsa:

1. Run the usual configuration tool for the program, such as a ``./configure`` script or ``cmake``, if any.

.. tip::

   The C front end of Mopsa is based on ``clang``.
   It is highly recommended to use it to compile the project, which is generally done with ``./configure CC=clang``.

2. Run ``mopsa-build`` by giving the usual build command along with its arguments (make sure that the ``mopsa-build`` script is visible in your ``$PATH``).
   For instance, if the program is normally built with ``make``, you will call:

   .. code-block:: shell-session

      $ mopsa-build make

   but any shell command can be wrapped (e.g., ``mopsa-build ./my/build/script -my-arg``).

   As a result of this workflow, the program to be analyzed will actually be built first.
   The ``mopsa-build`` tool intercepts the calls to the C compiler and linker to build a database of these calls, before passing the call to the legitimate build tool.
   The database automatically records the list of all the source files that need to be included in the analysis, together with the pre-processing and compiler options to pass to the front end.

   The database is stored by default into the ``mopsa.db`` file.
   This file uses a binary format, but can be inspected with the :ref:`mopsa-db <mopsa-db>`  tool.

3. Analyze the program by calling ``mopsa-c`` and passing it the ``mopsa.db`` file instead of C sources files.

   .. code-block:: shell-session

      $ mopsa-c mopsa.db


.. note::

   ``mopsa-build`` only recognizes a few build tools and their most common options.
   This is sufficient in many cases, and works well notably with autoconf build systems.
   Additionally, as it builds the project, it should handle automatically generated code well.

   But it may not work in all situations, see the :ref:`troubleshooting section <mopsa-build-troubleshooting>`.


Example
-------

To illustrate a basic usage of ``mopsa-build``, we analyze the sources of ``GNU time``:

.. code-block:: shell-session

   $ wget https://ftp.gnu.org/gnu/time/time-1.9.tar.gz
   $ tar xaf time-1.9.tar.gz
   $ cd time-1.9
   $ ./configure CC=clang
   $ mopsa-build make
   $ mopsa-c mopsa.db

which gives:

.. raw:: html
   :file: ../resources/output/time.out.html



Advanced Use
------------

The behavior of ``mopsa-build`` is controlled by two environment variables, explained below:

.. program:: mopsa-build

.. envvar:: MOPSADB

   Location of the database [#f1]_.
   If not set, the ``mopsa.db`` file in the current directory is used.

.. envvar:: MOPSADBLOG

   Set to 1 to enable (verbose) logs.
   All log information are stored in the mopsa.log text file.
   The effect of new commands are appended at the end.


Multi-Target Software
+++++++++++++++++++++

When the build command compiles several targets, the ``mopsa.db`` file contains the information for all the targets.
It is necessary to specify to ``mopsa-c`` the name of the target to analyze using the ``-make-target`` option.

.. code-block:: shell-session

   $ mopsa-c mopsa.db -make-target=<target>

The list of possible targets in a database can be listed by calling ``mopsa-db`` (the tool will list the full path of the built targets, but specifying the file name without the full path is sufficient when there are no ambiguities).

Note that, although each target corresponds to an executable built with ``mopsa-build``, Mopsa analyses the target from its sources files solely.
The executable binary is not used during the analysis at all; only the process of collecting the source files that made up the executable is of interest to Mopsa.


Cleaning
++++++++

Each call using the ``mopsa-build`` wrapper adds new information to the ``mopsa.db`` file, if it exists.
To clean up the database and start again from scratch (e.g., after a ``make clean``), simply delete the ``mopsa.db`` file.



Database Location
+++++++++++++++++

By default, the database is named ``mopsa.db`` and is stored in the directory where ``mopsa-build`` was called from.
The file name and path can be changed (generally to an absolute path name) by setting the ``MOPSADB`` envrironment variable.


.. _mopsa-build-troubleshooting:

Troubleshooting
+++++++++++++++

``mopsa-build`` currently intercepts calls to only a few compilation tools, such as ``cc``, ``gcc``, ``clang``, ``ld``, ``ar``, and a few file tools, such as ``cp``, ``mv``, ``rm``, ``ln`` (as it needs to track where object and library files as they are moved around).
The way it is done is by prefixing the ``$PATH`` with a directory, ``bin/mopsa-wrappers``, containing alternate versions of these commands; these are all symbolic links to the ``mopsa-wrapper`` binary, that uses ``Sys.argv.(0)`` to know which tool is wrapped.

In order to debug build issues, you can enable verbose logs by setting the ``MOPSADBLOG`` environment variable to ``1``.
The log information is stored in the ``mopsa.log`` text file.

You can also inspect the contents of the ``mopsa.db`` file with the :ref:`mopsa-db <mopsa-db>`  tool.

.. [#f1] While less intuitive than a command-line option, the use of an environment variable is required because the location of the database must be available to all calls to the wrapper, even when embedded inside complex build sequences (recursive calls to ``make``, shell scripts, etc.).
