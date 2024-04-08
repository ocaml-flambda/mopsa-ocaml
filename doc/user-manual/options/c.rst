.. _c-options:

C Options
=========

``mopsa-c`` supports the analysis of C programs with most features of the C99 standard and a large part of the standard C library (see the :ref:`analysis limitations <limitations>`).

This section describes C-specific domains and command-line options.
However, ``mopsa-c`` also accepts :ref:`general options <general-options>`, as well as :ref:`universal options <uni-options>` (possibly depending on which domains are activated by the chosen :ref:`configuration <confs>`).


Front end
---------

``mopsa-c`` uses a front end based on `Clang <https://clang.llvm.org/>`_ in order to parse C source files.
The front end assumes the current host as compilation target (which influences choices such as the size of types, endianess, pointer alignment, etc.).
The front end supports the following options that are available with all C configurations:

.. program:: mopsa-c

.. option:: -I <dir>

   Add the directory ``<dir>`` to the search path for header files.

   Note that, as Mopsa uses Clang as a front end, default system include paths (such as ``/usr/include``) are implicitly searched (as it would for a C compilation).


.. option:: -ccopt <opt>

   Pass the option ``<opt>`` to the Clang front end.

   Possible uses include defining a preprocessor symbol, as in ``-ccopt -DSYMBOL`` or ``-ccopt -DSYMBOL=value``.
   Not that, if the preprocessor option contains several words separated by spaces, then ``-ccopt`` must be repeated before each new word.

.. option:: -c-entry <function>

   Name of the entry point to analyze (default: ``main``).

.. option:: -c-symbolic-args <min>[:<max>]

   Set a value or a range for the number of command-line arguments given to ``main`` (default: ``1:32766``).

   The function ``int main(int argc, char* argv[])`` is analyzed by assuming that ``argc`` is an integer between two bounds, and ``argv`` is a ``NULL``-terminated array of ``argc`` strings.
   The precise value of ``argc`` and the length of each string are actually symbolic values: the analysis takes into account all possible combinations of argument numbers between the bounds, of string size, and string contents.
   This allows covering a large range of program behaviors during the analysis.

   The option allows setting either a fixed value ``-c-symbolic-args nb`` or a range ``-c-symbolic-args min:max`` for the number of command-line arguments.
   Not that ``argc`` actually equals one plus the number of command-line arguments.
   The effect of this option is exemplified in our :ref:`C benchmarks <c-benchs>` on Coreutils.

.. option:: -make-target <target>

   Name of the compiled target to analyze in a compilation database.

   This option is used in combination with a compilation database ``mopsa.db`` when the database contains several targets.
   See :ref:`the mopsa-build tool <mopsa-build>` for more details.

.. option:: -disable-parser-cache

   Disable the cache of the Clang parser.

   Unless this option is specified, the Clang-based parser stores pre-parsed AST of the compilation units (compiled C files) it encounters and reuses them when possible.
   An AST is stored as a file with the ``.mopsa_ast`` extension.
   This allows speeding up the front end when analyzing several times the same program or analyzing several programs that share some source files (this is for instance the case of our :ref:`C benchmarks <c-benchs>` on Coreutils).


.. option:: -Wall

   Output Clang's warnings that are generated during parsing.

.. _c-stub-options:

C Library Stubs
---------------

:ref:`Stubs <c-stubs>` allow specifying the behavior of unknown functions.
While stubs can be written in C enriched with :ref:`built-ins <c-builtins>`, Mopsa features a specific :ref:`contract-based language <c-contracts>` to describe stubs.
A large part of the C standard library is already available as stubs, which can be found in the :mopsa:`share/mopsa/stubs/c/libc/` directory.

Header inclusion follows the usual rules of C compilation.
Hence, when encountering  an ``#include <...>`` directive, Mopsa will include the original header file found in your distribution at the usual place (``/usr/include``, etc., although the ``-I`` option can alter this).
When detecting a standard library header inclusion, Mopsa will automatically add to the analysis the corresponding C stub file available in :mopsa:`share/mopsa/stubs/c/libc/`.
The :mopsa:`share/mopsa/stubs/c/mopsa/` directory contains stub files for Clang and Mopsa built-ins, that are not tied to a header file and are always added to the analysis.
Provided that stubs are indeed available for all the functions called by the program, analyzing a program using the C library should be completely transparent and require a minimal setup (e.g., using :ref:`the mopsa-build tool <mopsa-build>`).
See the :ref:`stub section <c-stubs>` for more details on the dedicated stub modelization language.

.. option:: -use-stub <fun1>,...

   Set the list of functions for which the contract stubs are used instead of the declarations when both are available (default: empty).

   By default, when a stub contract is available for a function that also has a C definition, the C code is used, unless the function is listed in this option.

.. option:: -without-libc

   Disable contract stubs of the standard C library.

Parts of the contract analysis engine which are not specific to C but could be used by other languages are actually implemented as Universal domains.
See the :ref:`relevant section in Universal <uni-stub-options>` for the shared stub contract options.

.. _cells-options:

Cells
-----

The ``c.memory.lowlevel.cells`` domain is an abstraction of C memory blocks that represents scalar fields as independent variables, called *cells* [LCTES06]_.

The domain has a low level view of memory blocks as collections of bytes that can be freely addressed with dereferences of any type.
The domain handles transparently type-punning arising from union types and pointer casts.
Here, a memory block is either a variable or a dynamically allocated memory block (as returned by, e.g., ``malloc``).

A limitation of the domain (and by extension of the current C analysis with Mopsa as all configurations use this domain) is that, as all accesses within the bounds of a memory block are considered valid, it *does not report some classes of undefined C behaviors*, such as writing to a union type using a field and reading back through another field, or overflowing an array embedded inside a structure as long as the addressed memory stays within the bounds of the structure.

Blocks can be considered either in expansion (full field sensitivity), or smashed into a single cell to improve performance (field insensitivity).

The domain supports the following options:

.. program:: c.memory.lowlevel.cells

.. option:: -cell-deref-expand <int>

   Set the maximal number of expanded cells when dereferencing a pointer (default: ``1``).

.. option:: -cell-smash

   Activate the on-demand smashing when the expansion threshold is reached.
   This option is currently limited to smashing pointer cells only.


.. _string-length-options:

Strings Length
--------------

The domain ``c.memory.lowlevel.string_length`` is an another abstraction of C blocks that keeps track of the position of the first `'\\0'` byte in the block, which is useful to validate C strings and compute their length [SAS18]_.

The domain supports the following option:

.. program:: c.memory.lowlevel.string_length

.. option:: -c-track-string-length <bool>

   Track the lengths of dynamic strings (default: ``true``).

   When set to false, the domain will only track the length of literal strings (which is less resource intensive but sufficient in some cases).


.. _machine-options:


Machine Numbers
---------------

The domain ``c.memory.scalar.machine_numbers`` handles numeric errors.
It relies on the presence of other numeric domains in the configuration to maintain integer and floating-point values and perform arithmetic computations.
These are generally the :ref:`integer interval domain <itv-options>` and the :ref:`floating-point interval domain<float_options>` from Universal (possibly in combination with others).

.. program:: c.memory.scalar.machine_numbers

Integer Arithmetic
~~~~~~~~~~~~~~~~~~

The machine number domain implements the wraparound semantics of C integers.
It translates the C arithmetic into unbounded mathematical arithmetic (handled by Universal numeric domains), and doing so, it detects possible overflows.
The domain has options to control whether overflows are reported or not (some overflows are reported by default and not others).
In all cases (overflows reported or not), the analysis continues with the modular arithmetic: no execution traces are pruned due to integer overflows (it means that you can disregard the alarm and still trust the analysis results in case the wraparound behavior was intended).

.. option:: -c-check-signed-arithmetic-overflow <bool>

   Report overflows in signed integer arithmetic (default: ``true``).

.. option:: -c-check-unsigned-arithmetic-overflow <bool>

   Report overflows in unsigned integer arithmetic (default: ``false``).

.. option:: -c-check-explicit-cast-overflow <bool>

   Report overflows in explicit casts (default: ``false``).

.. option:: -c-check-signed-implicit-cast-overflow <bool>

   Report overflows in implicit casts to signed integer (default: ``true``).

.. option:: -c-check-unsigned-implicit-cast-overflow <bool>

   Report overflows in implicit casts to unsigned integer (default: ``true``).


Floating-Point Arithmetic
~~~~~~~~~~~~~~~~~~~~~~~~~

:ref:`Float domains in universal <float_options>` handle floating-point arithmetic with proper rounding and special numbers, such as infinities and NaN.
It does not distinguish between silent and signaling NaNs, and between positive and negative zeros, though.
The floating-point rounding mode is currently controlled by :ref:`options in Universal <float_options>`.

By default, float errors such as overflows, divisions by zero, and invalid operations silently generate special numbers, which are then propagated without raising alarms.
However, the machine number domain has C-specific options to turn the generation of special numbers into alarms, if needed:

.. option:: -c-check-float-division-by-zero <bool>

   Float divisions by 0 generate alarms instead of infinities (default: ``false``).

   If enabled, the analysis continues with only the floating-point values that did not trigger a division by 0, and the result does not contain infinities.

.. option:: -c-check-float-invalid-operation <bool>

   Invalid float operations generate alarms instead of silent NaN (default: ``false``).

   If enabled, the analysis continues with only the floating-point values that did not trigger an invalid operation, and the result does not contain NaN.

.. option:: -c-check-float-overflow <bool>

   Float overflows generate errors instead of infinities (default: ``false``).

   If enabled, the analysis continues with only the floating-point values that did not trigger an overflow, and the result does not contain infinities.


.. _pack-options:

Packing Scope
-------------

The domain ``c.memory.packing.static_scope`` implements a simple packing strategy that splits the numeric environment into smaller packs, which is useful to increase the efficiency at the cost of precision when using relational domains: relations are then inferred only between variables within the same pack, but not between distinct packs.
It has no effect on non-relational numeric domains, such as intervals.
The packing strategy is specific to C, but it is intended to be used with a :ref:`relational domain <apron-options>` which is generic and provided by Universal.

Packing is done with respect to the scope of the variables.
The global variables are kept in a single pack, while the local variables of each function are kept in separate packs.
In addition, users can define their own packs using the option:

.. program:: c.memory.packing.static_scope

.. option:: -c-pack <v1>,<v2>,...,<vn>

   Create a custom pack by specifying a ``,``-separated list of variables.
   The following special notations are possible:

   - ``%<function>``: all local variables in ``<function>``.
   - ``%<function>.<var>``: local variable ``<var>`` in ``<function>``.
   - ``@<resource>``: all resource variables in ``<resource>`` (resources are defined and used in :ref:`C stub contracts <c-contracts>` to model, e.g., dynamic memory allocation and file descriptors).


.. _sentinel-options:

Pointer Sentinel
----------------

The domain ``c.memory.lowlevel.pointer_sentinel`` is similar to the string length domain, but it targets pointer arrays and is able to track the position of the first ``NULL`` pointer.
It is particularly useful to track the length of ``NULL``-terminated arrays, such as the ``argv`` string array.
When used with a relation domain, it enables Mopsa to track the relationships between the value of ``argc`` and the position of the ``NULL`` pointer in ``argv``.

.. program:: c.memory.lowlevel.pointer_sentinel

.. [LCTES06] Antoine Miné: `Field-Sensitive Value Analysis of Embedded C Programs with Union Types and Pointer Arithmetics. <https://www-apr.lip6.fr/~mine/publi/article-mine-lctes06.pdf>`_ LCTES 2006: 54–63.
.. [SAS18] Matthieu Journault, Antoine Miné, Abdelraouf Ouadjaout: `Modular Static Analysis of String Manipulations in C Programs. <https://www-apr.lip6.fr/~mine/publi/article-journault-al-sas18.pdf>`_ SAS 2018: 243–262.
