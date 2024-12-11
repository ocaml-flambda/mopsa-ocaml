.. _general-options:


General Options
===============

We list here the general options common to all analyses, that are not tied to a specific language or abstract domain.


Help
----

.. program:: Help

.. option:: --help, -help, -h

   Display the list of options.

   Without a configuration, the list of all options for all known domains and languages is shown.
   With a configuration, only the options for the enabled domains and language are listed.
   The ``-format json`` option influences the output (useful for front ends).

.. option:: -list (domains | checks | hooks)

   List the domains, checks performed, or hooks available.

   If a configuration is specified, only the domains and checks relevant to the configuration are shown.
   Note that, even if reporting of a specific alarm is disabled by a command-line option (or unless a command-line option is specified), the corresponding check *will* still be listed with ``-list checks``.
   This is because checks are associated to domains that are able to report them, and the list depends only on the list of domains, i.e., the chosen configuration.

   The ``-format json`` option influences the output (useful for front ends).



General
-------

.. program:: General

.. option:: -v

   Show the version of Mopsa.

.. option:: -config <file>

   Path to the :ref:`configuration <confs>` file to use for the analysis.

   The configuration file specifies the analyzed language, the set of domains used, and their relationships.
   It is written in a :ref:`JSON format <json-confs>`.
   Configuration files are stored in the :mopsa:`share/mopsa/configs/` directory and organized by language.

.. option:: -hook <hook>

   Activate a hook.

   Use ``-list hooks`` to list the available hooks.
   See also a list of :ref:`useful hooks <hooks>` and their options.

.. option:: -share-dir <directory>

   Specify the path to the directory containing the shared files, including configuration files and stub files.

   Configuration files are searched in ``<directory>/configs/``.
   Stub files are searched in ``<directory>/stubs/c/`` for C and ``<directory>/stubs/python/`` for Python.
   The ``mopsa`` script sets this path to ``../share/mopsa`` relative to the Mopsa binary (``mopsa.exe``).
   This default choice is consistent with both using Mopsa after installation and using Mopsa within the source tree without installation.
   Using ``-share-dir`` is mandatory when using the ``mopsa.bin`` binary directly.
   When using wrappers (such as ``mopsa``, ``mopsa-c``, etc.), it can be used to override the default choice.


.. option:: -cache <int>

   (Internal option).
   Set the size of the cache used during analysis (default: ``5``).

   Mopsa uses a cache internally to avoid redundant computations of post-conditions and evaluations.

.. option:: -clean-cur-only

   (Internal option).
   Flag to apply cleaners on the current environment only.


Alarms
------

These options change how checks and alarms are displayed.

.. program:: Alarm

.. option:: -show-callstacks

   Display the call stacks when reporting alarms in text format.

.. option:: -show-safe-checks

   Also show safe checks when reporting alarms in text format, in addition to failed checks.

   
Debugging
---------

.. program:: Debug

.. option:: -debug <c1>,<c2>,...,<cn>

   Enable some debug channels (default: ``print``).

   Debug information in Mopsa are organized into *channels*, which generally correspond to an abstract domain or an OCaml module.
   Channels can be enabled selectively.
   Use ``_`` as a wildcard to enable all channels.
   The ``print`` channel (enabled by default if no ``-debug`` option is specified) prints the effect of ``_mopsa_print`` directives inserted in the C source to show the abstract value of some variables.
   Other channels are generally only useful for debugging Mopsa.

.. option:: -engine (automatic | interactive | dap)

   Select the interaction mode with the analysis (default: ``automatic``).

   By default, the analysis is carried fully automatically, without user intervention, but alternate interaction modes are possible:

   - The ``interactive`` mode provides a gdb-like shell to run the analysis step by step, inspect the abstract state, place breakpoints, etc.
     It is described in more details in :ref:`this section <interactive>`.

   - The ``dap`` mode is a *work in progress* to support the `Debug Adapter Protocol <https://microsoft.github.io/debug-adapter-protocol/>`_, used notably in `Visual Code Studio <https://code.visualstudio.com/>`_.


Output
------

.. program:: Output

.. option:: -no-color

   Disable color in text output.

   By default, the analyzer uses ANSI codes to show colors, unless a *dumb* terminal is detected (``TERM`` set to ``dumb``) or this option is used.

.. option:: -no-warning

   Disable warning messages.

.. option:: -format (text | json)

   Select the output format (default: ``text``).

   The JSON output is particularly useful for post-processing by a UI or script (e.g. :ref:`mopsa-diff <mopsa-diff>`).
   This option influences the output of an analysis as well as the output of the ``-help`` and ``-list`` commands.

.. option:: -output <file>

   Redirect the output to a file.

   When redirecting the text output to a file for future processing, the ``-no-color`` option can be useful.

.. option:: -silent

   Always return a zero code, even if alarms are detected  (default: unset).

   If this option is not specified, a return code of 1 is used to denote the presence of alarms.

.. option:: -lflow

   Print the full abstract state at the end of the analysis (default: unset).

   The abstract state when ``main`` returns is displayed as if the ``_mopsa_print()`` primitive was called.
