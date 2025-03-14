.. _json-confs:


Configuration File Format
=========================

This section describes the JSON format of the configuration to help you modify or create new configurations.
Example configurations are available in the sub-directories of :mopsa:`share/mopsa/configs`.


Syntax
------

A configuration is described as a JSON file that is passed to Mopsa with the option ``-config``.
The syntax of a configuration is given by the following grammar:

.. literalinclude:: ../resources/syntax/configuration.bnf
   :language: bnf

A configuration specifies the target language to analyze, the employed abstract domains and how they are combined.


Language
--------

Accepted languages include:

- ``c`` for C,

- ``python`` for Python,

- ``universal`` for the Universal toy-language.


Domains
-------

In the configuration file, the domains are designated by their name (a string), as returned by the command ``mopsa -list domains``.

There are two kinds of domains:

- stand-alone *leaf domains*, which include abstractions, such as ``universal.numeric.values.intervals.integer``, and stateless iterators, such as ``c.iterators.loops``,

- and *functors*, which decorate a domain to build another domain with added functionality (an example is the ``c.memory.packing.static_scope`` domain, which splits the environment into small packs of variable to better scale relational numeric domains).

Leaf domains and functors are further distinguished into:

- environment domains, simply called *domains*, that directly abstract sets of program environments,

- and *value domains*, which abstract sets of values and are lifted systematically to environments as *non-relational domains* by associating an abstract value independently to each variable.



Semantic
--------

Every domain or combination of domains can be optionally tagged with ``semantic`` tag, which is a unique string denoting boundaries between sub-languages known and used internally in domain implementations.
Abstract analysis in Mopsa works through transformations that lower complex expressions into simpler ones until they can be effectively handled.
It is sometimes useful to limit the sequence of transformations until expressions of a certain form are reached, which is understood as belonging to a sub-language of the original language.

For instance, in a C analysis, in addition to the full C, two sub-language semantics are used internally in the analysis:

- ``C/Scalar`` denotes a subset of C expressions with only scalar values (integers, floats, pointers), no dereference, no side-effect, no function call,

- ``Universal`` denotes Universal expressions (without wrap-around nor integer or floating-point errors).

Thus, in a C configuration, one specific domain is tagged with ``C/Scalar`` and another is tagged with ``Universal``.
It indicates that, when reaching the domain, expression transformation should be considered complete for the corresponding sub-language.
Note that most domains have no ``semantic`` tag, as they denote only one of the many possible transformation required to achieve a well-named sub-language.


Leaf Domains
-------------

A leaf (environment or value) domain in the configuration can be denoted simply as a string (its name), when there is no ``semantic`` associated.

Alternatively, a ``domain`` JSON object is used, with the domain name as value and a ``semantic`` attribute.


Domain Combiners
----------------

Domains are combined together in a configuration using a set of operators:

* ``apply`` applies the functor domain with the specified name on a domain.

* ``product`` creates a reduced product over a list of domains. The optional property ``reductions`` sets the list of reduction rules used to refine the product (assumed to be empty if not specified). The concretization of a product is the intersection of the concretizations of its member domains.

* ``switch`` creates a cartesian product in which transfer functions of domains are called in sequence until one domain returns a reply. The concretization of a switch is the cartesian product of the concretizations of its member domains.

* ``compose`` behaves similarly to ``switch``, however the concretization is different. The concretization of each member domain depends on the concretization of the subsequent domains. In other words, each domain can be considered as a functor over the subsequent domains. However, the difference with a classic functor operator ``apply`` is that ``compose`` constructs a DAG instead of a tree, which allows *abstraction sharing*. For example, ``compose(product(D1,D2),D3)`` creates a DAG in which both ``D1`` and ``D2`` can lift the same instance of ``D3``, meaning that ``D3`` can infer invariants of both ``D1`` and ``D2`` at the same time. This is more precise than ``product(apply(D1,D3),apply(D2,D3))``.


Value Combiners
---------------

It is also possible to build a non-relational value abstraction using the ``nonrel`` operator.
This operator lifts a value abstraction into a domain by mapping variables to abstract values.
Value abstractions can be combined using the following operators:

* ``apply`` applies the value functor domain with the specified name on a value domain.

* ``product`` constructs a reduced product of value abstractions. The optional property ``reductions`` lists the reduction rules used by the product (assumed to be empty if not specified). The concretization of the product is the intersection of the concretizations of its member abstractions.

* ``union`` constructs a disjoint union of value abstractions (useful when differently-typed variables are abstracted using different value domains, such as integers and floats).


Example
-------

We illustrate the JSON format on the ``c/cell-string-length-pack-rel-itv-congr.json`` configuration example:

.. literalinclude:: ../resources/code/cell-string-length-pack-rel-itv-congr.json
   :language: js
   :linenos:

This is a configuration for a complex C analysis including classic interval abstractions, congruences, string lengths, and packed relational numeric domains.

- At the top-level, the configuration uses a composition (line 4) of a C-specific part (lines 5-56), on top of a Universal set of domains (lines 57-94).
  Note that the second part of the top-level composition is tagged with the ``Universal`` semantic tag (line 58), indicating that, when reaching these domains, the expressions do not feature C-specific constructions anymore.

- Each part contains a ``switch`` with a large sequence of domains.
  Each domain handles a specific case of expression or statement independently from the other domains, and Mopsa simply selects the (only) domain with a successfully match on the AST.
  Many domains correspond to state-less iterators (e.g., lines 7-15).
  Others maintain a simple state (e.g., file resources at line 19, or recency abstraction at line 31) or perform some check (e.g., format checking at line 20).

- The C-specific part also contains a composition (line 33) of a sub-part handling structured values and a sub-part handling scalar values.

  - Structured C values (lines 35-42) can be abstracted using the cell domains and, for character arrays, the string length domain as well.
    As both domains can be used simultaneously for some variables, a ``product`` is used (instead of a ``switch``), which includes a reduction.

  - Scalar C values (lines 45-49) can be either pointers or numeric values.
    These are handled by different domains, hence we use a ``switch`` to tell Mopsa to pick the first domain with a successful match.
    Note that, when entering the scalar part of the composition, we know that expressions have been transformed into C expressions manipulating only scalars, hence we tag the ``switch`` with the ``C/Scalar`` semantic.

  - It is important to compose the domains handling C structures and C scalars with a ``compose`` combinator because both C structure domains can generate expressions with new scalar variables (e.g., variables representing cell values or string lengths).
    We want the subsequent scalar abstraction to maintain a single shared abstract environment representing both kinds of variables, and note instantiate a distinct environment abstraction for each variable kind.

- The Universal part handles only generic iterators and numeric abstractions on mathematical integers and floats.

  - The numeric environment is a ``product`` (line 67) of a non-relational abstraction (lines 69-82) and a relational abstraction (lines 85-86) using a reduction (line 90).

  - The non-relational abstraction lifts (line 69) value domains.
    These consist in a disjoint ``union`` (line 70) of domains handling integers (lines 73-79) and floats (line 71).
    Integer handling is itself a ``product`` (line 73) of intervals (line 74) and congruences (line 75) with a classic value reduction (line 78).

  - The relational abstraction uses variable packing, which is handled as a packing function (line 85) applied to a classic relational domain (line 86).

  - As before, it is important that the C-specific part and the numeric Universal part are combined using a ``compose`` operator (line 4) as some C domains generate new variables (cell and string length domains, as discussed before, but also the size of some resources such as dynamically allocated blocks at line 26) which are combined in an environment that is ultimately abstracted together in a shared numeric environment.
    This ensures, for instance, that the relational domain (line 86) can maintain relations between variables denoting string lengths (line 37), pointer offsets (line 47), integer variables (line 48), size of malloc-ed blocks (line 26), etc.
