.. _reports:

Reports
=======

At the end of an analysis, Mopsa prints a *report* that summarizes the *checks* it performed, the corresponding *diagnostics*, and the possible *assumptions* made and threats to soundness.
Reports are in human-readable form by default, but can also be output in JSON format.

.. _checks:

Checks
------

During the analysis, the abstract domains perform a number of *checks* to verify the correctness of some safety properties.
The list of checks supported by an analysis is language-dependent (and even :ref:`configuration <confs>`-dependent), and can be obtained with the option ``-list checks``.
For instance, on C:

.. code-block:: shell-session

  $ mopsa-c -list checks
  Checks:
    Assertion failure
    Stub condition
    Invalid memory access
    Division by zero
    Integer overflow
    Invalid shift
    Invalid pointer comparison
    Invalid pointer subtraction
    Double free
    Insufficient variadic arguments
    Insufficient format arguments
    Invalid type of format argument
    Invalid floating-point operation
    Floating-point division by zero
    Floating-point overflow

and on Python:

.. code-block:: shell-session

  $ mopsa-python -list checks
  Checks:
    Assertion failure
    Uncaught Python exception

Some checks can be enabled and disabled using command-line options, and some checks (such as floating-point errors) may not be enabled by default.
See the :ref:`C analysis options<c-options>` for instance for the list of options pertaining to C run-time errors.


Diagnostics
-----------

A diagnostic is the outcome of a check at some program location.
Three outcomes are possible:

``Safe``
  The check is valid for all execution flows reaching the program location.

``Error``
  The check is invalid for all executions flows reaching the program location.

``Warning``
  The validity of the check cannot be determined.
  This may correspond to an actual error in the program for some execution flows, or to a spurious warning caused by a too coarse abstraction.

By default, Mopsa prints only errors and warnings.
Safe checks can be display with the option ``-show-safe-checks``.


.. _assumptions:

Soundness and Assumptions
-------------------------

During an analysis, the hypotheses under which the analyzer is sound and takes into account all possible executions may suddenly not longer hold.
This is the case, for instance, if the program calls an undefined function, uses an unsupported language feature, or, in certain cases, due to a catastrophic loss of precision in the abstraction (such as being unable to track the possible targets of a pointer after a complex operation).
In those cases, Mopsa nevertheless continues the analysis, with a best-effort strategy.
In the final report, Mopsa indicates explicitly that the analysis may not be sound and lists the assumptions it made during the analysis.

For instance, the following program :download:`assumption.c <../resources/code/assumption.c>` calls an undefined function ``f``:

.. literalinclude:: ../resources/code/assumption.c
   :language: c
   :linenos:

Running ``mopsa-c assumption.c``, we get the following report:

.. raw:: html
   :file: ../resources/output/assumption.c.out.html

- The first line, ``Unsound analysis`` (replacing ``Analysis terminated successfully``), indicates that at least one assumption was made.
- The last line explains the assumption. The analysis assumed that the undefined function ``f`` simply returned an arbitrary value in the range of the return type without any side-effect (modification of other variables, errors).

Mopsa is careful to explicitly report any possible threat to soundness.
It is then the responsibility of the user to check whether the assumptions were indeed correct or to fix the analysis (e.g., by providing the source code from ``f``).


File Output
-----------

The output can be redirected to a file with the ``-output FILE`` option.


Return Value
------------

The analyzer exits with a 0 return code when the analysis completes without any warning or error, and 1 otherwise.

The ``-silent`` option causes the analysis to always return 0, even in case of error.



JSON Output
-----------

By default, Mopsa uses a human-readable output in colored text (unless the ``-no-color`` option is used).
Alternatively, Mopsa can export the analysis report in JSON format, using the option ``-format=json``.
This is useful for post-processing the analysis result with a tool or GUI, and it is generally used in combination with the ``-output`` option to output into a file.

The JSON output is structured as follows:

- The boolean field ``success`` indicates whether the analysis terminated successfully.
  The analysis may fail due to an unsupported language feature or due to an internal error.
  In such cases, an ``exception`` field contains the error message.

- The float field ``time`` contains the total analysis duration.

- The fields ``mopsa_version`` and ``mopsa_dev_version`` allow retrieving the Mopsa version used in the analysis.

- The field ``files`` contains the list of analyzed files.

- When ``success`` is ``true``, the field ``alarms`` contains the list of potential alarms detected during the analysis.
  Each alarm is structured as follows:

  - The field ``title`` is a normalized string describing the alarm kind (as returned by the ``-list checks`` option).
  - The field ``messages`` gives additional, free-form information on the cause of the alarm, such as the variables involved, the values of the offending arguments, etc.
  - The field ``range`` indicates the positions in the program that surround the offending statement or expression. It is composed of two positions ``start`` and ``end``. Each position has fields ``file``, ``line``, and ``column`` (line numbers start at 1, columns at 0).
  - The field ``callstacks`` contains callstacks, that is, lists representing the traces of function calls from the entry point until the offending statement. Each element of a callstack has:

    - The field ``function`` representing the name of the called function.
    - The field ``range`` representing the location of the call site to this function.

    The field may contain several callstacks, in case the alarm location can be reached by several different sequences of calls during the execution.

- The field ``assumptions`` lists the soundness assumptions that the analysis made, if any.

Here is an example JSON output as returned by ``mopsa-c -format=json hello.c``, using the analysis of :download:`hello.c <../resources/code/hello.c>` from the :ref:`basic usage <usage>` section:

.. literalinclude:: ../resources/output/hello.c.json
   :language: json
   :linenos:
