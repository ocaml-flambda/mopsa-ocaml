.. _c-contracts:

C Contract Language
===================

This section describes the *contact language* developed to specify functions at an abstract level, compactly, and without writing C code.
Mopsa provides ready-to-use stubs for a large part of the GNU C library; they are written in this language and can serve as example.
They can be found in :mopsa:`share/mopsa/stubs/c/libc` (note that they will be automatically included in the analysis when the relevant header file is included).

The precise formal semantics of the language as well as examples and experiments are presented in a publication [SAS20]_.

.. warning::

   Mopsa's contract language is similar to other Behavioral Interface Specification Languages, such as Frama-C's ACSL.
   However, our aim is different: such languages are generally used to write specifications of C functions in order to verify (often using deductive methods) that the source code of the function obeys the specification; in Mopsa, the contract is used solely *instead* of the function source code, in order to check the client code without the function source.
   Our language aims at writing stubs for abstract interpreters.

Language Syntax
---------------

A contract for a function must be written in *special comments* ``/*$ ... */`` just *before the declaration* of the function.
The syntax of the language is as follows:

.. code-block:: bnf

   <Contract> ::= (<Stmt> | <Case>)*

   <Case>     ::= case <String> { <Stmt>* }

   <Stmt>     ::= <Effect> | <Cond>

   <Effect>   ::= local   : <Type> <Id> = <Value>;
              |   assigns : <Expr> <Interval>*;
              |   free    : <Expr>;
              |   warn    : <String>;
              |   unsound : <String>;

   <Cond>     ::= assumes : <Formula>;
              |   requires: <Formula>;
              |   ensures : <Formula>;

   <Formula>  ::= <Expr>
              |   <Expr> in <Set>
              |   true
              |   false
              |   <Formula> and <Formula>
              |   <Formula> or <Formula>
              |   <Formula> implies <Formula>
              |   not <Formula>
              |   forall <Type> <Id> in <Interval>: <Formula>
              |   exists <Type> <Id> in <Interval>: <Formula>
              |   <Formula> otherwise <Expr>
              |   if <Formula> then <Formula> else <Formula> end
              |   if <Formula> then <Formula> end

   <Set>      ::= <Interval>
              |   <Resource>

   <Interval> ::= [<Expr>, <Expr>]
              |   (<Expr>, <Expr>]
              |   [<Expr>, <Expr>)
              |   (<Expr>, <Expr>)

   <Value>    ::= new <Resource>
              |   <Id> (<Expr>,..., <Expr>)

   <Resource> ::= <Id>

A contract specifies conditions on the input arguments for the call to be valid, as well as the returned value and how the call modifies its environments.
It consists in a sequence of *statements* containing *formulas*.
Each statement starts with a keyword followed with a colon ``:`` and ends with a semicolon ``;``.
Statements include: :ref:`pre-conditions <stub-pre>` (``requires``), :ref:`post-conditions <stub-post>` (``ensures``, ``modifies``, ``free``), :ref:`local contract variable <stub-locals>` (``local``), :ref:`messages <stub-msg>` (``warn``, ``unsound``), and contracts can be split into :ref:`cases <stub-cases>` (``case``, ``assume``), as detailed in the rest of this section.
First, a few notes about the syntax:

- Formulas ``<Formula>`` use the classic logic connectors: ``and``, ``or``, ``not``, ``implies``, the constants ``true`` and ``false``, as well as :ref:`quantifiers <stub-quantifiers>` ``forall`` and ``exists``.

- :ref:`Expressions <stub-expr>` ``<Expr>`` appearing inside formulas are side-effect free C expressions, and can access all available C variables, notably the formal arguments of the function and global variables. In some circumstances (detailed below), they are enriched with primed variables ``'`` and built-in functions.

- Variables ``<Id>`` and :ref:`resources <stub-resources>` ``<Resource>`` can be any valid C identifiers.

- Types ``<Type>`` can be any valid C types, built-in or previously declared. The C syntax is slightly adapted to avoid ambiguities between variable names and type names.

- ``<String>`` denote arbitrary C string literals, enclosed in ``"``.

- :ref:`Intervals <stub-intervals>` ``<Interval>`` denote consecutive sequences of integers.

To simplify formulas, it is also possible to define :ref:`predicates <stub-predicates>` (some of which are already predefined in :mopsa:`share/mopsa/stubs/c/mopsa/mopsa.c`), as described below.
They are introduced using ``/*$= ... */`` style comments.
Finally, it is possible to execute :ref:`contracts globally <stub-global>`, at the beginning of the analysis, when global variables are declared, using ``/*$! ... */`` style comments.

.. _stub-pre:

Pre-Conditions
--------------

The statement ``requires : <Formula>;`` adds a pre-condition.
An alarm ``Stub condition`` is raised if the pre-condition is not satisfied for some execution traces.
A contract can include several ``requires`` statements, in which case there will be an alarm for each failed statement.
It is also possible to combine several conditions into a single statement with ``and`` and a get a single alarm.
After an alarm, the evaluation of the contract continues with only (an over-approximation of) the set of execution traces where the condition holds.

The ``otherwise`` connector allows evaluating an expression instead of raising the standard alarm when the condition does not hold.
It is mainly useful in combination with the ``raise(<String>)`` built-in function to control more finely the error message: ``f otherwise raise("msg")`` will highlight the ``raise("msg")`` part of the formula if ``f`` does not hold.
It is possible to write a more complex expression after ``otherwise``, which is mainly useful to further specialize the error, as in:

.. code-block:: text

   requires : valid_ptr(p)
              otherwise (p == NULL ? raise("NULL pointer") : raise ("invalid pointer"));

where ``valid_ptr(p)`` is a predefined :ref:`predicate <stub-predicates>` requiring that the dereference ``*p`` is valid.

.. _stub-post:

Modified Variables and Post-Conditions
--------------------------------------

When variables are modified, the contract must:

- specify which variables (and possible which part) are modified with ``assigns`` statements,

- optionally add assumptions on the new value, with ``ensures`` statements.

An ``assigns`` statement is followed with a C lvalue: it can be a variable ``v``, a pointer dereference ``*p``, a structure access ``s.field``, etc.
Possible modified memory locations include global variable, as well as memory reachable from function arguments and global variables (arguments are passed by value, so, modifying them has no effect on the caller).
When the lvalue denotes an array, it is possible to specify an index range of modified elements using an :ref:`interval notation <stub-intervals>`, such as ``[expr1, expr2]``.
Any memory location that is not explicitly marked as modified by the contract is *assumed to be unchanged* by the call.

The ``ensures`` statement is followed with a formula that is assumed to hold when the function returns.
For modified variables, to distinguish between the value at the beginning of the call and at the end of the call, we use a prime ``'`` suffix for the later.
It is possible, for instance, to specify the range of a modified variable ``*p`` with ``ensures : *p >= 0 and *p <= 10;``, or to give a relation between its value before and after the call with ``ensures : (*p)' == (*p) + 1;``, which denotes an incrementation.
It is an error to use a prime for a memory location not explicitly specified with an ``assigns`` statement.
If, however, a memory location specified with an ``assigns`` statement is not constrained with an ``ensures``, it keeps a non-deterministic value within the full range of its type.
The prime can be used at the end of complex lvalues, such as ``(a[i])'``.
Note that ``(a[*p])'`` denotes the new value of array ``a`` at the index specified by the old value of ``*p``, while ``(a[(*p)'])'`` uses the index specified by the new value of ``*p`` (which should as be constrained in an ``ensures`` statement beforehand).
Only ``ensures`` statements can reference primed (i.e., post-condition) variables.
An alternative to the prime ``'`` notation is the ``primed`` built-in: ``primed(expr)`` is equivalent to ``(expr)'``.
Any number of ``assigns`` and ``ensures`` statement can be specified.

As example, the following contract states that ``f`` increments its argument passed by pointer, provided that the memory pointed to is addressable:

.. code-block:: c

   /*$
    * requires : valid_ptr(x);
    * assigns  : *x;
    * ensures  : (*x)' == (*x) + 1;
    */
   void f(int* x);

.. warning::

   Classic mistakes when writing contracts include:

   - forgetting to specify all the modified locations with ``assigns``,

   - forgetting to prime variables ``'`` in the  ``ensures`` post-condition; a condition on non-primed variables in an ``ensures`` statement restricts the environment at the *entry* of the function and can cut important execution traces without warning as, unlike ``requires`` statements, traces that are cut in ``ensures`` do not raise any alarm;

   - writing formulas that are not satisfiable or are too constrained in ``ensures`` statements; this can also result in important traces being cut without warning.

Return Value
------------

The ``ensures`` statement can also be used to specify the value returned by the function, using the ``return`` special variable.
For instance:

.. code-block:: text

   ensures : return >= 0 and return <= 1;

No prime (nor ``assigns``) is necessary for ``return``, it exists only in the post-condition.
Unless the ``return`` variable is constrained with an ``ensures`` statement, a non-deterministic value in the whole range of the return type is assumed to be returned.

.. _stub-expr:

Expressions and Built-Ins
-------------------------

Expressions obey mostly the C syntax.
There are however a few adaptations to ease parsing:

- casting expression ``e`` to type ``t`` can be written ``cast(t) e``; this is necessary if ``t`` is a typedef name as, otherwise, the parser assumes it denotes a variable;

- the ``sizeof`` operator must be written either ``sizeof_type(t)`` or ``sizeof_expr(e)``, depending on whether the byte size of a type ``t`` or of an expression ``e`` is requested.

The syntax is also enriched with several built-ins, that can be used anywhere in expressions:

- ``raise(<String>)``, only allowed in ``requires`` statements, is used to report alarms;

- ``primed(lval)``, only allowed in ``ensures`` statements, is equivalent to ``(lval)'`` and denotes the value of ``lval`` at the end of the function;

- ``return``, only allowed in ``ensures`` statements, denotes the value returned by the function;

- ``bytes(expr)``, when ``expr`` is a pointer, is the total size, in bytes, of the memory block the pointer is pointing into (the offset of the pointer is irrelevant, the size is counted from the beginning until the end of the memory block, even if the pointer points into the middle of the block);

- ``offset(expr)`` is the offset, in bytes, from the beginning of the memory block; hence, ``bytes(p) - offset(p)`` is the number of bytes available between the current position of ``p`` and the end of the block it is pointing into;

- ``base(expr)`` points to the first byte (offset 0) of the memory block ``expr`` is pointing into;

- ``size(expr)`` is similar to ``bytes(expr)`` but, assuming that ``expr`` has type ``t*``, then the size is expressed as a number of elements of type ``t`` instead of bytes; i.e., ``size(p) == bytes(p) / sizeof(t)``;

- ``index(expr)`` is similar to ``offset(expr)`` but, as ``size(expr)``, counts as a number of elements of type ``t``; hence, ``size(p) - index(p)`` is the number of elements of type ``t`` available between the current position of ``p`` and the end of the block;

- ``valid_float(expr)`` returns 1 if ``expr`` is a valid floating-point value (not infinity nor NaN), 0 otherwise;

- ``float_inf(expr)`` returns 1 if ``expr`` is an infinity floating-point value, 0 otherwise;

- ``float_nan(expr)`` returns 1 if ``expr`` is a NaN floating-point value, 0 otherwise;

- ``resource(expr)``, where ``expr`` is a pointer, returns 1 if the pointer points into a memory managed by the :ref:`resource system <stub-resources>` (such as a dynamically allocated block);

- ``alive(expr)``, where ``expr`` is a pointer, returns 1 if the memory block (or more generally the :ref:`resource <stub-resources>`) it points into has not been freed, 0 otherwise.

  .. note::

     An important aspect of Mopsa's contract language is that C expressions use C types and retain their *classic C semantics*.
     In particular, integer arithmetics in C expressions is assumed to wrap-around as modular arithmetics.
     This is in contrast to contract languages that use purely logical formulas, with unbounded mathematical integers.

     This choice is generally convenient to model C functions.
     However, beware possible unintended overflows in formulas (e.g., in :ref:`interval bounds <stub-intervals>`)!

.. _stub-intervals:

Intervals
---------

Intervals denote ranges of consecutive integers.

Intervals of the form ``[expr1, expr2]`` have both bounds included.
Alternatively, intervals ``(expr1,expr2]``, ``[expr1,expr2)``, and ``(expr1,expr2)`` have, respectively, the lower bound, the upper bound, and both bounds excluded.

.. warning::

   These alternate notations are very useful to write ranges such as ``[0,size)``, when ``size`` is an expression of unsigned type.
   By contrast, ``[0,size-1]`` has  a risk of overflow with unsigned wrap-around in case ``size`` is zero, giving a large range instead of the expected empty range!

.. _stub-quantifiers:

Quantifiers
-----------

Formulas can feature the ``forall`` and ``exists`` quantifiers.

In ``forall type v in interval: formula`` (and similarly for ``exists``), a variable ``v`` of the specified type is introduced and available in the subsequent formula.
Mopsa can only quantify over integer variables, with explicit bounds ``interval``.

As example, the following statement requires as pre-condition that ``s`` (assumed to be of type ``char*``) points to a valid C string:

.. code-block:: text

   requires : exists size_t i in [0, bytes(s) - offset(s)): s[i] == 0;

i.e., there is a zero byte between the current position of ``s`` and the end of the memory block it points into.
The following statements ensure in a post-condition that the memory region starting at ``p`` and of size ``n`` is zero-initialized:

.. code-block:: text

   requires : valid_bytes(p, n);
   assigns  : p[0, n);
   ensures  : forall size_t i in [0,n): (p[i])' == 0;

As explained later in the section, ``valid_bytes(p, n)`` is a predefined :ref:`predicate <stub-predicates>` that requires that ``p`` indeed points to a valid memory with at least ``n`` bytes available.

.. _stub-resources:

Resources
---------

Resources in Mopsa are a generic way to model dynamic objects that can be allocated and freed.
Examples include dynamic memory managed by ``malloc``, ``realloc``, and ``free``, but also system resources such as files and directory streams, file descriptors, etc.
Resources are viewed as memory blocks: when allocated, a resource provides a pointer to the beginning of the block, that can be manipulated as any data pointer in C (supporting dereferences, pointer arithmetics, etc.).
In addition to a memory block, each resource is associated upon allocation a *class*, which serve two purposes: firstly, it is possible to check the resource class of a pointer to ensure that it is used with the right API (e.g., memory allocated with ``mmap`` should be freed with ``munmap`` and not by ``free``); secondly, abstract domains in Mopsa can associate specific semantics to certain classes (such as read-only memory, or file resources, as detailed below).

Resource Allocation
+++++++++++++++++++

Resources are allocated with the ``new <Resource>`` syntax, where the resource class ``<Resource>`` can be any valid C identifier.
Resources should be stored immediately into a :ref:`local variable <stub-locals>` of pointer type.
Upon allocation, the size of the memory block is undefined.
If the pointer is to be dereferenced, then its size should be set, using the ``size`` built-in, in the post-condition.
The following examples show the allocation of a memory block of size ``n``, which is returned by the contract:

.. code-block:: c

   /*$
    * local   : void* var = new Memory;
    * ensures : size(var) == n;
    * ensures : return == var;
    */
   void* alloc(int n);

As the local variable ``var`` does not exist in the pre-condition, there is no need to use the ``'`` notation when using it in the ``ensures`` statements.
It would also be possible to initialize the memory in the post-condition with additional ``ensures`` statements.

Once allocated, the size of the block cannot be changed (hence, ``realloc`` proceeds by allocating a new block, which is returned after copying the old contents and freeing the old block, raising an alarm as expected if the old block is used afterwards).

Resource Freeing
++++++++++++++++

In order to free memory, the ``free : <Expr>;`` statement is used, where ``<Expr>`` must evaluate to a pointer in the memory block of a resource.
It is not necessary to point to the beginning of the memory block, pointing a some non-zero offset in the block is sufficient to free the whole block.
Accessing the block after ``free`` raises an ``Invalid memory access`` alarm; freeing again the resource raises a ``Double free`` alarm.

Resource Functions
++++++++++++++++++

It is often necessary to query whether some pointer denotes a valid resource of a certain kind, which is possible using the following built-ins and syntax in pre-condition formulas.

- ``resource(expr)`` is true if ``expr`` evaluates to a pointer pointing into a block managed by the resource system (even if the resource has been freed);

- ``expr in <Resource>`` is true if ``expr`` points into a resource of the specified class (alive or freed);

- ``alive(expr)`` is true if ``expr`` points into a resource that has not been freed.

In all cases, as with ``free``, ``expr`` can point anywhere within the block, not necessarily at the beginning.
Hence, a more complete way to free a memory block would be:

.. code-block:: c

   /*$
    * requires : p in Memory otherwise raise("pointer not allocated by alloc");
    * requires : alive(p) otherwise raise("double free");
    * requires : offset(p) == 0 otherwise raise("pointer not at beginning of block");
    * free : p;
    */
   void free(void *p);

More complete models of dynamic memory management functions are available in :mopsa:`share/mopsa/stubs/c/libc/stdlib.c`.

Pre-Defined Resource Classes
++++++++++++++++++++++++++++

The following classes are known to Mopsa and used in the C library stubs:

``Memory``
    Memory blocks managed by ``malloc`` and freed with ``free`` (which include also the memory allocated by ``strdup``, etc.).

``File``
    Resources for ``FILE*`` pointers returned by the ``fopen`` family of functions.

    The resource system is used to ensure the proper use of the file stream API (passing ``FILE*`` pointers allocated with ``fopen``, no double ``fclose``, etc.).
    The memory block associated with the resource has ``sizeof(FILE)`` bytes, so that we can use ``FILE`` fields to store state information useful to model the API.
    It is notably used to remember the file descriptor associated with the open steam (see the ``_alloc_FILE`` function in :mopsa:`share/mopsa/stubs/c/libc/stdio.c`).

``FileRes``
    Resources associated to open file descriptors, as managed by the ``open`` family of functions.

    File descriptors are managed as integers in the C library.
    To use the resource system to handle file descriptors symbolically and track the open status of files, we shadow file descriptors with a ``FileRes`` resource, which is created when a file is open, and freed when the file is closed.
    A specific abstract domain in Mopsa, ``c.libs.clib.file_descriptor``, maintains the relationship between ``FileRes`` resources and integers actually returned by ``open``.
    The domain is also aware of the specific allocation policy for file descriptors: the C library returns the smallest unused integer.
    In some circumstances, the domain is able to provide a precise, concrete file descriptor value, instead of a symbolic integer with unknown value.
    For instance, the domain can discover that, after the following C code, ``f`` is actually zero:

    .. code-block:: c

       close(0);
       int f = open(...);

    This is indeed a common pattern in C programs.

    .. todo::

       ``ReadOnlyMemory``: is memory protection implemented?

.. _stub-locals:

Local Variables
---------------

Contracts can feature local variables, with two specific uses:

- storing pointers to memory blocks from freshly allocated resources, using:

  .. code-block:: text

     local : <Type>* <Id> = new <Resource>;

- calling another function and storing its return value, using:

  .. code-block:: text

     local : <Type> <Id> = <Id>(<Expr>, ..., <Expr>);

  The assigned expression is necessarily a direct function call, the second ``<Id>`` being the name of the called function.
  The function may be another contract or a regular C function.
  In particular, the :ref:`built-in functions <c-builtins>` provided by :mopsa:`share/mopsa/stubs/c/mopsa/mopsa.h` help greatly to write more compact contracts.
  For instance:

  .. code-block:: text

     local : char* res =  _mopsa_new_string();

  calls the ``_mopsa_new_string`` :ref:`built-in function <c-builtins>`, which allocates a block of ``Memory`` class and ensures that it contains a string of arbitrary length and contents, but properly zero-terminated.

A local contract variable can be used in the contract after its declaration and until the last statement of the contract.
No prime is needed to use  it in ``ensures`` statement as it does not exist in the pre-condition can cannot be confused with a pre-condition value.
It is destroyed at the end of the evaluation of the contract.

.. _stub-cases:

Cases
-----

Contracts support disjunctive behaviors, where functions behave differently depending on the value of some argument, on the current state, or have non-deterministic behaviors.

A contract can feature any number of *cases*, introduced with the ``case`` statement.
Each case has a *name*, which is a string literal typeset within ``"``, followed with a sequence of statements grouped within curly brackets.
Cases support an additional type of statements, ``assumes : <Formula>;``, that indicate under which pre-conditions the case must be executed.
All the following statements within the case (including ``requires``, ``assigns``, ``ensures``, ``local``, etc.) are only executed for the execution traces that satisfy the ``assumes`` formulas.
Like ``requires``, ``assumes`` reasons on the pre-condition (without primed variables) but, unlike ``requires``, does not issue alarms for the traces that do not satisfy the condition.
All the statements outside the ``case`` statements are executed for all cases, which allows factoring the common behavior.
When executing the contract, each case is first evaluated independently (mixing the common statements and the case-specific statements), and then the result of all the cases are joined.
It is possible for an execution trace to satisfy several cases (in case of deliberate overlap, non-deterministic behavior, or some imprecision in the evaluation of the ``assumes`` statements).
In that case, a post-condition for each enabled case is computed, and they are all joined in the final post-condition.

Generally, a disjunctive contract is written as:

- a set of common ``requires`` that state the pre-conditions that must hold for the call to be correct or raise adequate alarms;

- a set of cases comprising each:

    - ``assumes`` statements that restrict the pre-condition for this case;

    - possible additional ``requires`` that trigger case-specific alarms within the restricted pre-condition;

    - post-condition statements (``assigns``, ``ensures``, etc.) for this case;

- a set of common post-condition statements for common behaviors.

Note that local variables inside a case are specific to that case.
Moreover, cases cannot be nested.

In the following example, we refine the contract for our allocation function by stating that allocating a zero-sized block returns ``NULL``, and allocating a block of size at least 1 can non-deterministically return ``NULL`` and set ``errno``:

.. code-block:: c

   /*$
    * requires : n >= 0;
    *
    * case "zero" {
    *   assumes : n == 0;
    *   ensures : return == NULL;
    * }
    *
    * case "OK" {
    *   assumes : n > 0;
    *   local   : void* var = new Memory;
    *   ensures : size(var) == n;
    *   ensures : return == var;
    * }
    *
    * case "error" {
    *   assumes : n > 0;
    *   ensures : return == NULL;
    *   assigns : errno;
    * }
    */
   void* alloc(int n);

.. warning::

   It is your responsibility to ensure that the case assumptions are disjoint, if this is the desired behavior (there is no implicit *else* between consecutive cases).
   Also ensure that the case assumptions cover all the pre-condition states that do not raise an alarm; otherwise, it is possible for some execution traces to fail all conditions and never generate a post-condition, i.e., the trace is silently dropped.

It is possible to disable some cases by name on some contract using the ``-stub-ignore-case <function>.<case>`` :ref:`option <uni-stub-options>`.
For instance, ``-stub-ignore-case alloc.error`` would disable the non-deterministic error case in our ``alloc`` function.

.. _stub-msg:

Messages
--------

The following statements will trigger error messages whenever they are executed:

- ``warn : <String>;`` prints a warning message (such messages are printed, unless the ``-no-warning`` option is used, but do not count as alarms);

- ``unsound : <String>;`` indicates that the analysis is unsound and adds the string message to the list of soundness assumptions that are reported at the end of the analysis.

These are useful, for instance, to indicate that a stub is not complete, or a function should not be called.

.. _stub-predicates:

Predicates
----------

It is possible to define predicates that are then used in contracts, in order to simplify them.
Predicate definitions are typeset within special comments ``/*$= ... */``.
They obey the following syntax:

.. code-block:: bnf

   <Contract> ::= <Predicate>*

   <Predicate> ::= predicate <Id>(<Id>, ... <Id>): <Formula>;

A predicate can have any legal C identifier as name, and an arbitrary number of arguments that are used in the subsequent formula.
A predicate instantiation ``<Id>(<Expr>, ..., <Expr>)`` can be used everywhere a formula is requested.
The predicate is then replaced with its definition, and the argument names are substituted with the expressions passed as argument.

For instance, the following code defines a predicate to check that ``s`` points to a valid string:

.. code-block:: c

   /$*=
    * predicate valid_string(s):
    *   valid_ptr(s) and
    *   exists size_t i in [0, bytes(s) - offset(s)): s[i] == 0;
    */

Then, a contract can type ``requires : s == NULL or valid_string(s);`` to check concisely that ``s`` is either ``NULL`` or a valid string.

The file :mopsa:`share/mopsa/stubs/c/mopsa/mopsa.c` already defines a set of predicates that are used throughout the C library stubs.
We mention here a few that could prove useful to write custom stubs.

Memory Predicates
+++++++++++++++++

- ``valid_base(p)`` is true if ``p`` points into a valid memory block (however, its offset can be outside the block bounds);

- ``valid_base_or_fail(p)`` raises an alarm when ``p`` does not point into a valid memory block;

- ``valid_ptr(p)`` is true if ``p`` is a valid pointer, i.e., it points at a valid offset into a valid memory block, so that ``p`` can be safely dereferenced;

- ``valid_ptr_or_fail(p)`` raises an alarm if ``p`` is not a valid pointer;

- ``null_or_valid_ptr(p)`` is true if ``p`` is valid or ``NULL``;

- ``null_or_valid_ptr_or_fail(p)`` raises an alarm if ``p`` is neither valid nor ``NULL``;

- ``valid_ptr_range(p, i, j)`` is true if all locations from ``p[i]`` to ``p[j]`` are valid;

- ``valid_ptr_range_or_fail(p, i, j)`` raises an alarm unless all locations from ``p[i]`` to ``p[j]`` are valid;

- ``valid_bytes(p, n)`` is true if ``p`` is a valid pointer and there are at least ``n`` bytes available starting at its location;

- ``valid_bytes_or_fail(p, n)`` raises an alarm unless at least ``n`` bytes are available starting at location ``p``;

- ``null_or_valid_bytes(p, n)`` is true if ``p`` is ``NULL`` or has ``n`` bytes available;

- ``null_or_valid_bytes_or_fail(p, n)`` raises an alarm unless ``p`` is either ``NULL`` or has ``n`` bytes available;

- ``in_bytes(r, x, n)`` is true if ``r`` points into the memory starting at ``x`` and of size ``n`` bytes.

String Predicates
+++++++++++++++++

- ``valid_string(s)`` is true if ``s`` points to a valid C string, i.e., there is a zero byte between ``s`` and the end of the memory block ``s`` is pointing into;

- ``valid_string_or_fail(s)`` raises an alarm unless ``s`` points to a valid C string;

- ``null_or_valid_string(s)`` is true if ``s`` is ``NULL`` or points to a valid C string;

- ``null_or_valid_string_or_fail(s)`` raises an alarm unless ``s`` is ``NULL`` or points to a valid C string;

- ``valid_primed_string(s)`` is used in ``ensures`` to add as post-condition that ``s`` is a valid C string: there is a zero byte between ``s`` and the end of the block when the function returns (the memory starting at ``s`` must also either appear in an ``assigns``, or be a newly allocated block);

- ``valid_substring(s, n)`` is true if ``s`` is a valid C string of length at most ``n``, i.e., there is a zero byte within the ``n`` bytes following ``s``;

- ``valid_substring_or_fail(s, n)`` raises an alarm unless ``s`` is a valid C string of length  at most ``n``;

- ``valid_primed_substring(s, n)`` used in ``ensures`` to ensure that ``s`` points to a string of length at most ``n`` in the post-condition;

- ``in_string(x, s)`` is true if ``x`` points within the string starting at ``s``, i.e., there is no zero byte between ``s`` and ``x``;

Resource Predicates
+++++++++++++++++++

- ``alive_resource(p, r)`` is true if ``p`` is a resource of class ``r`` and has not been freed.

.. _stub-global:

Global Variable Contracts
-------------------------

It is sometimes useful to use the contract language to specify the value of global variables.
It is possible to provide global contracts within ``/*$! ... */`` comments.
Such contracts can feature post-condition statements, such as ``assigns`` and ``ensures``, but also ``local`` to create resources, and are executed after the global variables are created and before the entry point function is executed.

For instance, the following code ensures that the ``program_invocation_name`` string, defined by the GNU C library, is properly initialized with a constant string before the program starts:

.. code-block:: c

   char *program_invocation_name;

   /*$!
    * local: char* addr = _mopsa_new_readonly_string();
    * assigns: program_invocation_name;
    * ensures: program_invocation_name' == addr;
    */

Where ``_mopsa_new_readonly_string`` is a built-in function defined using a contract in :mopsa:`share/mopsa/stubs/c/mopsa/mopsa.c`.

.. [SAS20] Abdelraouf Ouadjaout, Antoine Miné: `A Library Modeling Language for the Static Analysis of C Programs. <https://www-apr.lip6.fr/~mine/publi/ouadjaout-al-sas20.pdf>`_ SAS 2020: 223–246.
