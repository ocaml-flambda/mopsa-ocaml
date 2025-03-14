.. _c-builtins:

C Built-Ins
===========

To help writing analysis-specific C helper code and stubs, Mopsa provides a set of built-in functions.
They are available through the ``#include "mopsa.h"`` include directive, which includes the file :mopsa:`share/mopsa/stubs/c/mopsa/mopsa.h`.

Interestingly, while a few functions are actually built-ins handled specifically by abstract domains in Mopsa, many of them are actually defined using the :ref:`contract language <c-contracts>`.
Their source code is available in :mopsa:`share/mopsa/stubs/c/mopsa/mopsa.c`.


Values
------

Value built-ins allow generating non-deterministic values corresponding to basic types.
By soundness, the analysis will consider every possible execution for any possible value in the non-deterministic range.
Non-deterministic values are thus especially useful to model unknown inputs.

Integers
++++++++

The following functions generate integer values within their full range, according to specific bit-size and signedness:

.. code-block:: c

   signed char _mopsa_rand_s8();
   unsigned char _mopsa_rand_u8();

   signed short _mopsa_rand_s16();
   unsigned short _mopsa_rand_u16();

   signed int _mopsa_rand_s32();
   unsigned int _mopsa_rand_u32();

   signed long _mopsa_rand_s64();
   unsigned long _mopsa_rand_u64();

The following functions additionally take a lower and an upper bound (included):

.. code-block:: c

   signed char _mopsa_range_s8(signed char l, signed char u);
   unsigned char _mopsa_range_u8(unsigned char l, unsigned char u);

   signed short _mopsa_range_s16(signed short l, signed short u);
   unsigned short _mopsa_range_u16(unsigned short l, unsigned short u);

   signed int _mopsa_range_s32(signed int l, signed int u);
   unsigned int _mopsa_range_u32(unsigned int l, unsigned int u);

   signed long _mopsa_range_s64(signed long l, signed long u);
   unsigned long _mopsa_range_u64(unsigned long l, unsigned long u);

Floating-Point Numbers
++++++++++++++++++++++

The following functions generate floating-point values (including valid floats, NaN, and infinities) for some floating-point type:

.. code-block:: c

   float _mopsa_rand_float();
   double _mopsa_rand_double();

The following functions generate only valid floating-point values (i.e., not NaN, nor infinities) in the full range of some floating-point type:

.. code-block:: c

   float _mopsa_valid_float();
   double _mopsa_valid_double();

The following functions generate valid floating-point values within two bounds (included):

.. code-block:: c

   float _mopsa_range_float(float l, float u);
   double _mopsa_range_double(double l, double u);

Pointers
++++++++

This function returns a non-deterministic pointer:

.. code-block:: c

   void *_mopsa_rand_void_pointer();

The pointer is assumed to point to any valid or invalid location (⊤).
Because it can point to an invalid location, dereferencing the pointer for writing or reading will trigger an ``Invalid memory access`` alarm.
Because it can also point to a valid location, but this location is not known to Mopsa, a write through this pointer will also trigger an unsoundness alert ``ignoring modification of blocks pointed by undetermined pointer``: the analysis continues, but ignores the possible modification to the memory (this behavior was preferred to the alternative consisting in continuing the analysis assuming that every memory location may have been modified, which would give poor results).
A read will not trigger an unsoundness alert, only a dereference error, and continue the analysis assuming a non-deterministic value within the type of the pointed-to element is read.

The following function returns an invalid pointer.
Dereferencing it will only trigger an ``Invalid memory access`` alarm, and the analysis will not continue for this execution trace.

.. code-block:: c
  
   void *_mopsa_invalid_pointer();


Printing
--------

The following function prints the abstract state:

.. code-block:: c

   void _mopsa_print();
   void _mopsa_print(var1, ..., varn);

The first form, with no argument, prints the whole abstract state (which can be very large).

In the second form, a list of variables is specified, and only the abstract information concerning these variables is printed.
Currently, the variables can also be side-effect free lvalues, such as ``s.f`` or ``a[i+1]``.
It is possible to print directly a structure or an array, in which case all the fields and elements will be printed.
Printing a pointer also prints the pointed-to value.


Assertions
----------

The ``_mopsa_assert`` function operates as a classic assertion mechanism: if ``cond`` can evaluate to 0, then an alarm is reported by Mopsa.
The analysis continues assuming that the assertion holds.
Hence, it can be used to check soundly functional properties.

.. code-block:: c

   void _mopsa_assert(int cond);

Three outcomes for ``_mopsa_assert`` are possible:

- Mopsa can prove that all execution traces satisfy the condition, then no alarm is reported and the analysis continues;

- Mopsa can prove that all traces reaching the assertion fail the condition, then an alarm is reported and the analysis stops for these traces;

- some traces satisfy the condition and others do not, or, due to over-approximation, Mopsa cannot conclude either way for some traces, then an alarm is reported and the analysis continues only for those traces where Mopsa cannot prove that the condition is always false.

Note that, due to over-approximations, Mopsa may keep a trace even if it does not actually satisfy the condition, and it can report spurious assertion violations.

In addition to this classic assertion, Mopsa features several assertions described below that are mostly used in unit-tests.
   
The ``_mopsa_assert_exists`` function raises an alarm if the condition cannot be true for any execution trace.
In all cases, the analysis continues with all the traces unchanged.
Due to over-approximations, it is possible that Mopsa fails to report that a condition cannot hold because there are spurious traces that do, or there is an uncertainty in the evaluation of the condition: it only checks that the over-approximation computed by Mopsa contains a certain behavior.
It is mainly useful to check the soundness of the analysis in controlled unit tests.

.. code-block:: c

   void _mopsa_assert_exists(int cond);

The following functions check that the over-approximation computed contains no trace, or contains at least one trace:

.. code-block:: c

   void _mopsa_assert_unreachable();
   void _mopsa_assert_reachable();

``_mopsa_assert_unreachable`` can be used to check soundly that some code is indeed unreachable, which is useful in program verification.
``_mopsa_assert_reachable`` is mainly useful to check the soundness of the analyzer, when it is known that the code should be reachable.

The following functions are also used in unit tests to check that, respectively, no alarm is raised, or at least one alarm is raised in the test.

.. code-block:: c

   void _mopsa_assert_safe();
   void _mopsa_assert_unsafe();

The ``_mopsa_assume`` function continues the analysis keeping only the execution traces that can satisfy the condition.
It keeps the same traces an assertion would but, unlike ``_mopsa_assert``, it does not raise any alarm, even when it cannot prove that the condition is true for all traces.
It can be used to model the environment or the effect of an unknown function by constraining a non-deterministic assignment with a complex condition.

.. code-block:: c
                
   void _mopsa_assume(int cond);

``_mopsa_assert`` and ``_mopsa_assume`` can be used in tandem to write simple pre- and post-conditions.
However, the :ref:`contract language <c-contracts>` has more advanced capabilities for hypotheses and for alarm reporting.


Memory
------

Mopsa has a set of built-ins useful to manipulate memory.

The following functions check that a pointer is valid and raise an alarm if it is not:

.. code-block:: c

   void _mopsa_assert_valid_ptr(void *p);
   void _mopsa_assert_valid_bytes(void *p, size_t n);

``_mopsa_assert_valid_ptr(p)`` checks that at least one byte can be dereferenced at address ``p``, while ``_mopsa_assert_valid_bytes(p,n)`` checks that ``n`` bytes can be dereferenced starting at address ``p``.

The ``_mopsa_memrand(p,i,j)`` function is useful to put all the bytes at offsets ``i`` to ``j`` (included) from address ``p`` to non-deterministic values, raising an alarm if the byte range is not valid:

.. code-block:: c

   void _mopsa_memrand(char *s, size_t i, size_t j);

The following built-ins are similar to the corresponding C library functions:

.. code-block:: c

   void _mopsa_memset(char *s, char c, size_t i, size_t j);
   void _mopsa_memcpy(char *dst, char *src, size_t i, size_t j);

There is no built-in to allocate, resize, or free memory, as the standard C functions ``malloc``, ``realloc``, ``free`` can be used.


Strings
-------

Mopsa also features a set of built-ins to manipulate null-terminated C strings.

The following assertion emits an alarm if ``s`` cannot be proved to point to a null-terminated string (i.e., ``s`` is an invalid pointer, or there may not be a 0 byte between ``s`` and the end of the memory block ``s`` points into):

.. code-block:: c

   void _mopsa_assert_valid_string(char *s);

The following assertion ensures that ``s`` points to a null-terminated string with length at most ``n`` bytes (including the terminal 0 byte):

.. code-block:: c

   void _mopsa_assert_valid_substring(char *s, size_t n);


The following allocation functions allocate a string of unspecified size and contents, with as only guarantee that it is 0-terminated.
For the second function, a maximum size is specified; otherwise, it is bounded by ``INT_MAX``.

.. code-block:: c

   char *_mopsa_new_valid_string();
   char *_mopsa_new_valid_string_max(size_t max);

The string is allocated in the same memory pool as ``malloc``, and can thus be freed with ``free`` (as usual C string function do).

The following functions are similar, but allocate the string in a read-only memory pool.
An alarm is reported for any attempt to modify or free the string.

.. code-block:: c

   char *_mopsa_new_readonly_string();
   char *_mopsa_new_readonly_string_max(size_t max);

The following functions fill the memory block starting at ``s`` with a non-deterministic string, with size bounded by the end of the block for ``_mopsa_strrand`` and by ``n`` for ``_mopsa_strnrand``, ensuring that the string is 0-terminated:

.. code-block:: c

   void _mopsa_strrand(char *s);
   void _mopsa_strnrand(char *s, size_t n);


Files
------

The following functions check that the argument corresponds, respectively, to a file descriptor allocated with ``open``, or a ``FILE*`` stream  allocated with ``fopen``, that has not been closed yet:

.. code-block:: c

   void _mopsa_assert_valid_stream(void* stream);
   void _mopsa_assert_valid_file_descriptor(int fd);

The following built-ins are used internally in the model of the C library to convert between file descriptors (i.e., integers managed by the C library) and file resources managed by Mopsa's abstract domains (more on :ref:`resources <stub-resources>`  in the section about the contract language):

.. code-block:: c

   int   _mopsa_register_file_resource(void* res);
   int   _mopsa_register_file_resource_at(void* res, int fd);
   void* _mopsa_find_file_resource(int fd);



Errors
------

The following function aborts the analysis with an error message:

.. code-block:: c

   void _mopsa_panic(const char* msg);

Not to be confused with adding an alarm to the set of reported alarms without aborting the analysis, as done for instance with ``_mopsa_assert``.


Format Built-Ins
----------------

Mopsa treats the classic C function featuring formatting strings as built-ins.
This includes: ``printf``, ``fprintf`` and company, as well as ``scanf``, ``fscanf``, etc.
When the format string points to a literal string constant, this allows Mopsa to check that the argument number and types match the format, and report alarms when they do not.


Compiler Built-Ins
------------------

Clang features a number of built-ins.
Because these are used at different places in the standard C library headers, they are also supported by Mopsa.
We present here those that can be useful when writing stubs.
Unlike Mopsa specific built-ins, these do not require including any header.

The following built-ins test the special values of floating-point numbers and could be useful in assertions and assumptions:

.. code-block:: c

   int __builtin_isfinite(val);
   int __builtin_isnan(val);
   int __builtin_isnormal(val);

The following function returns 1 for infinity, -1 for negative infinity, and 0 for non-infinity numbers:

.. code-block:: c

   int __builtin_isinf_sign(val);

The following function returns one of the integer values passed as argument, depending on the class of the floating-point number ``val``:

.. code-block:: c

   int __builtin_fpclassify(fp_nan, fp_infinite, fp_normal, fp_subnormal, fp_zero, val);

The following function returns the bit sign (0 for positive, 1 for negative) of the floating-point number:

.. code-block:: c

   int __builtin_signbit(val);

The following functions return infinity in different floating-point types:

.. code-block:: c

   double __builtin_huge_val();

   float __builtin_huge_valf();
   float __builtin_huge_inff();

   long double __builtin_huge_vall();


Finally, the following function creates a NaN:

.. code-block:: c

   float __builtin_nanf(char*);

Mopsa support other built-ins used in the C library, such as the polymorphic comparisons: ``__builtin_isgreater``, ``__builtin_isgreaterequal``, ``__builtin_isless``, ``__builtin_islessequal``, ``__builtin_islessgreater``, ``__builtin_isunordered``; the predicates: ``__builtin_constant_p``, ``__builtin_expect``; the functions used to model internally variable argument lists: ``__builtin_va_start``, ``__builtin_va_end``, ``__builtin_va_copy``; stack allocation: ``__builtin_alloca``, ``alloca``.

The ``__builtin_unreachable()`` built-in is supported and raises an alarm if the point is reachable (which can be spurious as Mopsa computes over-approximations).


C11 atomic intrinsic instructions, such as ``_c11_atomic_load`` and others, are mapped to regular (non-atomic) operations, as Mopsa does not support concurrency at the moment.


