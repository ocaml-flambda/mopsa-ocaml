.. _confs:

Configurations
==============


The first step in setting up an analysis with Mopsa is choosing a *configuration*.
This is done using the ``-config`` option, although a default configuration can also be used if the option is not specified (which is what we did implicitly in our previous analyses).

Mopsa is based on the concept of *abstract domains*: modules that explain how to handle certain language constructs (such as loops or dynamic memory allocation) and provide alternate approximate representations of program states with various cost-precision trade-offs (such as keeping only lower and upper bounds of integer program variables).
To achieve a high degree of modularity and multi-language support, an analysis is composed of a large set of collaborating domains that can be swapped in and out.
This composition is described in more details in the publication [VSTTE19]_.

A configuration file in Mopsa specifies:

- the language analyzed (currently C, Python, or Universal),

- the list of abstract domains to use and their relationship.



Selecting a Configuration
-------------------------

``-config <lang>/<file>``

   Sets the configuration.

   Configuration files are kept as JSON files in language-specific sub-directories of :mopsa:`share/mopsa/configs/`.
   The ``-config <lang>/<file>`` option selects the configuration stored in ``share/mopsa/configs/<lang>/<file>``.


The JSON format used to describe configurations is presented in a :ref:`separate section <json-confs>`.
We focus here on using existing configurations.


.. note::

   ``mopsa-c`` and ``mopsa-python`` are actually wrappers around the main Mopsa script, ``mopsa``, which simply set a default configuration for the language:

   - ``mopsa-c`` is equivalent to ``mopsa -config c/default.json``

   - ``mopsa-python`` is equivalent to ``mopsa -config python/default.json``

     To use a non-default configuration, you can either call ``mopsa`` with the ``-config`` option, or add the ``-config`` option to ``mopsa-c`` or ``mopsa-python`` to override the default configuration (the ``-config`` option is mandatory with ``mopsa`` to perform an analysis as it has no default configuration).

The current default configuration for C selects a low-precision, efficient interval analysis, as well as all the necessary domains to handle C constructions and :ref:`C library stubs <c-stubs>`.
The current default configuration for Python selects a value analysis using also intervals.
See about other :ref:`common configurations <other-confs>` below and an :ref:`example C analysis <example-conf>` using an alternate configuration.



Option Categories
-----------------

Many command-line options are specific to an abstract domain.
In the following sections, we will organize these options by domain, being understood that the option is only relevant if the domain is enabled.
Some options and domains are language-specific, and will be presented in language-specific sections: :ref:`for C <c-options>`, :ref:`for Python <py-options>`, and :ref:`for Universal <uni-options>`.
Note that Universal is both a toy-language with its own syntax and analyzer (``mopsa-universal``), and a set of common domains that are reused by C and Python analyses. Hence, Universal options are also relevant for C and Python.
General options, not related to configurations, are described in :ref:`another section <general-options>`.

Because the list of available options depends on the language and enabled domains, and thus on the chosen configuration, you should use:

.. code-block:: shell-session

   $ mopsa -config=<lang>/<file> -help

to get the list of available options for a configuration.

Likewise, to get the list of domains enabled for a configuration, you can call:

.. code-block:: shell-session

   $ mopsa -config=<lang>/<file> -list domains

Calling ``mopsa -list domains`` without a configuration lists *all* the available domains, while ``mopsa -help`` will give the list of *all possible* options for all domains and languages.

Calling ``mopsa-c -help`` and ``mopsa-c -list domains`` respectively counts as calling ``mopsa -help`` and ``mopsa -list domains`` with a configuration, the default one for C, and so, they  only show the domains and options related to this configuration.

.. warning::

   The set of domains and their options are subject to change.
   This documentation may not be in sync with the latest development of Mopsa.



.. _example-conf:

Alternate C Configuration Example
---------------------------------

Consider the following program :download:`relation.c <../resources/code/relation.c>`:

.. literalinclude:: ../resources/code/relation.c
   :language: c
   :linenos:

An analysis with the default configuration will raise alarms:

.. raw:: html
   :file: ../resources/output/relation.c.out.html

Mopsa can prove that ``i`` is bounded, so that there is no overflow on ``i++`` nor array out-of-bound access in ``a[i]``.
However, it cannot prove that ``j`` is bounded and cannot go negative.
This causes an arithmetic overflow in the decrementation ``j--`` (going outside the range of integer type), and an illegal array access in ``a[j]``.
These are *false alarms*, as the operations ``j--`` and ``a[j]`` are actually safe and ``j`` stays positive.

The reason for this imprecision is that the default configuration, ``c/default.json``, which is a symbolic link to ``c/cell-itv.json``, employs an interval abstraction that can bound ``i`` (thanks to the test ``i < 10``) but cannot bound ``j``.
This is a case where a more powerful domain, a *relational domain*, is required, to infer the loop invariant ``i + j = 19`` and use it to deduce a bound on ``j`` from that on ``i``.
Thankfully, Mopsa includes a *linear relation domain* able to do just this.
It is not enabled in the default configuration, as it is more costly than plain intervals, but it is available in an alternate configuration: ``c/cell-pack-rel-itv.json``.
Hence, the following analysis:

.. raw:: html
   :file: ../resources/output/relation.c.with-rel.out.html

is successful in proving the absence of overflows and of out-of-bound array accesses: there are no alarms.


.. _other-confs:

Common Configurations
---------------------


We list some of the most interesting configurations currently available.
See the :mopsa:`share/mopsa/configs/` directory for the list of all available configurations and the :ref:`configuration format section <json-confs>` to see how to build your own.


.. warning::

   The set of domains and configurations is subject to change.
   This documentation may not be in sync with the latest development of Mopsa.


Common C Configurations
+++++++++++++++++++++++


:config:`c/cell-itv.json`
    This is the default configuration, which includes the minimum set of abstract domains for a working C analysis. The major domains we discuss in the following sections and contain interesting configuration options include : the :ref:`cell domain <cells-options>` (handling structured blocks), the :ref:`integer and float interval domains <itv-options>`, the :ref:`machine number domain <machine-options>` (taking care of integer wrap-around), the :ref:`heap recency domain <recency>` (handling dynamic memory allocation), an :ref:`inter-procedural analysis domain <interproc-options>`, a :ref:`loop handling domain <loops-options>`. The configuration also contains a number of domains that handle specific C constructs (such as switch, goto, etc.), and a number of domains to handle :ref:`stub contracts <c-contracts>` and C library concepts (such as file descriptors, format strings, etc.).

    Mopsa encourages the development of small, modular domains, so, even a simple and minimal configuration can contain a large number of them.

:config:`c/cell-itv-congr.json`
    This configuration adds the :ref:`congruence abstract domain <cong-options>` to the default interval analysis. Congruences infer integer value properties of the form :math:`a\mathbb{Z} + b`, which is useful to track precisely pointer alignment. In addition to the domain, the configuration adds a *reduction* that propagates information between intervals and congruences, allowing each domain to refine the value inferred by the other.

:config:`c/cell-string-length-itv-congr.json`
    This configuration adds to the previous configuration (intervals + congruences) a C-specific domain that tracks the :ref:`length of C strings <string-length-options>`, i.e., the number of bytes from the beginning of each character array until the occurrence of the first 0. The string length is abstracted using whatever numeric domains are included in the configuration, which in this case are intervals and congruences.

:config:`c/cell-string-length-pack-rel-itv-congr.json`
    This configuration adds to the previous configuration a :ref:`relational domain <apron-options>` that tracks linear relationships between numeric variables.
    It includes a reduction between linear invariants and intervals.
    As this configuration also tracks string lengths, it is able to infer linear relations between string lengths and the value of numeric variables.
    Because relational domains can be very costly when too many variables are related together, the configuration also includes a C-specific :ref:`packing strategy domain <pack-options>` which limits the relations to small packs of a few variables at a time.

:config:`c/cell-string-length-pointer-sentinel-pack-rel-itv-congr.json`
    This configuration adds the :ref:`pointer sentinel domain <sentinel-options>`, which is similar to the string length domain, but tracks instead the length of ``NULL``-terminated arrays of pointers (such as ``argv``).

:config:`c/cell-pack-rel-itv.json`
    This configuration contains only the interval domain and the linear relation domain (including variable packing and reduction).
    Note that a ``c/cell-rel-itv.json`` configuration, without packing, is also present, but it is discouraged except for the smallest programs as the lack of packing severely limits the scalability.


The effect of some of the different configurations can be observed, for instance, on our :ref:`C benchmarks <c-benchs>` on Coreutils.


Common Python Configurations
++++++++++++++++++++++++++++


:config:`python/values.json`
     This is the default configuration for Python analyses, which performs a value analysis. It includes the :ref:`integer and float interval domains <itv-options>`, a domain to represent sets of constant strings, as well as a large number of Python-specific domains handling the control, data-model, standard objects, and type system of Python programs.

:config:`python/types.json`
    This configuration removes the interval domains to achieve a (more efficient) type analysis instead of a value analysis.
    The string set domain is still kept to track precisely the set of attributes of objects as it is an important aspect of the structural type system in Python.

The effect of these various configurations can also be observed on our :ref:`Python benchmarks <py-benchs>`.


.. [VSTTE19] Matthieu Journault, Antoine Miné, Raphaël Monat, Abdelraouf Ouadjaout: `Combinations of Reusable Abstract Domains for a Multilingual Static Analyzer. <https://www-apr.lip6.fr/~mine/publi/article-mine-al-vstte19.pdf>`_ VSTTE 2019: 1–18.
