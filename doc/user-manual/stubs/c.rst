.. _c-stubs:

C Stubs
=======

Mopsa can only analyze *full C programs*, which requires that:

- an entry point is specified (a single entry-point, ``main`` by default, although a :ref:`unit test <unit-tests>` mode is available to run several ``test_`` functions in a row),

- and a definition for every C function that is called is provided.

In case the entry point function is not found, the analyzer stops with an error:

.. raw:: html
   :file: ../resources/output/no-main.out.html

If a called function is not found, the analyzer continues nevertheless, but reports a potentially unsound analysis, as follows:

.. raw:: html
   :file: ../resources/output/undefined-called.out.html

.. note::

   The possible unsoundness comes from the fact that the analyzer will consider that the function can return any value but has not side-effect (no modification of global variables nor memory reachable by the function arguments, no function call, no error).
   If this is not the case, more information on the undefined function is required, in the form of a stub.

A *stub* is a definition that is added to your program for the purpose of the analysis.
Stubs can be useful in order to:

- provide definitions for functions with missing C source, such as library functions (note that stubs for the :ref:`standard C library <c-stub-options>` are already provided and automatically used if needed during the analysis);

- provide alternate definitions for C functions that cannot be analyzed by Mopsa due to unsupported features (such as in-line assembly);

- provide alternate definitions for C functions that are too complex to analyze;

- write a library *driver*, i.e., a synthetic entry point that simulates an environment and the expected sequences of calls to the library, in order to verify the library without its client code; hopefully, the driver should be as general as possible, to cover all possible use cases;

- write :ref:`unit tests <unit-tests>` to analyze selected program parts, selected library functions, or stress-test the analyzer.

An important aspect is that the provided definitions may be abstracted: it is possible to introduce imprecision in stubs to simplify them.
This is easy thanks to a set of built-in non-deterministic functions provided by Mopsa, such as ``_mopsa_rand_s32``: when encountering non-deterministic choices, Mopsa will analyze every possible outcome, allowing us to request the systematic analysis of a large range of behaviors in a very compact way.

Examples
---------

Stubs can be written as C functions.
Alternatively, Mopsa provides a specific contract language to write stubs in a more logical form, using pre- and post-conditions instead of C code, which is often more compact (especially when reasoning at an abstract level).
The rest of this sections provides a few example stubs.
The following sections will present in more details the :ref:`built-in C functions <c-builtins>` available to write stubs and unit tests, and the :ref:`stub contract language for C <c-contracts>`.


Stubs as C code
+++++++++++++++

The following example uses :ref:`built-in C <c-builtins>` to write a stub for a function ``f`` taking as argument a buffer, its size, and a double value; it modifies the buffer up to the size, filling the rest with 0 bytes, and returns the number of bytes actually stored; in case of an error, -1 is returned and a global ``error`` variable is set:

.. literalinclude:: ../resources/code/stub-c-builtin.c
   :language: c
   :linenos:

Requirements on the arguments are expressed using ``_mopsa_assert``: the size ``size`` must be strictly positive and ``x`` must be a valid (non infinity nor NaN) float.
Moreover, ``_mopsa_assert_valid_bytes(buf, size)`` requires the buffer to contain at least ``size`` bytes.
Failure to meet the requirements will result in ``Assertion failure`` alarms, and the analysis continues with only the execution traces that obey the requirements.
Non-deterministic behaviors, to model errors, uses ``_mopsa_rand_s8``, which returns a random value in [-128, 127].
By soundness, Mopsa will consider that both branches can be executed for any call to ``f``.
Assumptions on the returned value (it must be less than ``size``) are expressed with ``_mopsa_assume``, and the buffer modification uses the ``_mopsa_memrand(buf, i, j)`` function, which modifies non-deterministically ``buf`` from byte offsets ``i`` to ``j`` (included), and ``_mopsa_memset(buf, c, i, j)`` which stores ``c`` bytes from offsets ``i`` to ``j`` (included) into ``buf``.

The analysis of the ``main`` function with Mopsa shows that there is no error.

The model is relatively compact thanks to built-in ``_mopsa_...`` functions.
In particular, ``_mopsa_assert_valid_bytes``, ``_mopsa_memrand``, and ``_mopsa_memset`` avoid the use of explicit loops to iterate on ``buf`` elements (but it is not always possible to avoid them).


Stubs as Contracts
++++++++++++++++++

We now show the same stub but using Mopsa's :ref:`contract language <c-contracts>`:

.. literalinclude:: ../resources/code/stub-c-contract.c
   :language: c
   :linenos:

(``size`` has been replaced with ``_size`` because the former is a reserved keyword in contracts).

The contract is typeset inside special comments ``/*$ ... */``.
Requirements are now typeset with the ``requires`` keyword.
To check that the buffer is large enough, the contract has access to meta-information about pointers such as ``bytes(buf)``, the byte-size of the memory block ``buf`` is pointing into, and its offset ``offset(buf)`` with respect to the beginning of the memory block.
The ``valid_ptr`` predicate ensures that ``buf`` is not ``NULL`` nor points to an invalid location (freed memory, etc.), while ``valid_float`` checks the value of a floating-point number.

The non-deterministic behaviors is expressed as two alternate cases with the ``case`` statement, both of which will be included in the analysis.
Every modified memory area is declared with the ``assigns`` keyword.
The ``ensures`` keyword can express some information about the memory state when the function returns, including the returned value (``return`` variable), the part of ``buf`` initialized to 0, the value of ``error``.
The prime ``'`` in ``error'`` and ``(buf[i])'`` indicates that the constraint is on the value of the expression when the function returns, not when it is called (we need to make this distinction because the function changes their values).

Note that the use of loops is replaced with a very compact quantified formula ``forall int i...``, which is one of the main benefits of contracts.
Actually, the ``_mopsa_memrand`` and ``_mopsa_memset`` built-ins, used in the last example to simplify the C stub, are defined using contracts with quantified formulas in :mopsa:`share/mopsa/stubs/c/mopsa/mopsa.c`.
Likewise, the ``_mopsa_assert_valid_bytes`` built-in is defined as a contract using the ``bytes`` and ``offsets`` functions to access pointer meta-information.


C Library Analysis Driver
+++++++++++++++++++++++++

The following example analyzes the C API for file streams:

.. literalinclude:: ../resources/code/stub-file.c
   :language: c
   :linenos:

The file stream is first open with an arbitrary file name (generated by ``_mopsa_new_valid_string``, which returns an unbounded string of non-deterministic contents) and checked for errors.
Then, an unbounded loop calls stream API functions in arbitrary order, using random but valid argument values.
Finally, the file is closed.

The non-deterministic nature of the test ensures that a large set of behaviors is covered by the analysis.
The analysis with Mopsa of this entry point returns no alarm with the :config:`c/cell-string-length-itv.json` configuration.


.. _c-unit-test:

C Unit Test
+++++++++++

The following example comes form the :ref:`unit tests <unit-tests>` available in Mopsa in :mopsa:`analyzer/tests/c/`.
It checks that the ``alloca`` function is correctly handled by Mopsa.

.. literalinclude:: ../resources/code/alloca_tests.c
   :language: c
   :linenos:

When run with the ``-unittest`` option, Mopsa will analyze both the ``test_safe`` and ``test_unsafe`` entry points and report errors.

In addition to a classic ``_mopsa_assert`` to check the correct values of the variables, the ``test_safe`` test uses ``_mopsa_assert_safe`` to ensure that the execution of the function does not raise any alarm.
By contrast, ``test_unsafe`` deliberately performs an illegal instruction, accessing a memory block allocated with ``alloca`` after it has been automatically freed by the called function, and uses ``_mopsa_assert_unsafe`` to check that Mopsa indeed discovered the issue.
Hence, the absence of alarm at the end of ``test_unsafe`` is actually a witness that Mopsa did raise an alarm at an earlier point in the function, as expected.
