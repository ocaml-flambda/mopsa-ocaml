.. _py-options:

Python Options
==============

The Python analysis is still a work in progress.
It supports a large subset of the core Python language, except for ``eval``, ``async``, meta-classes, and recursive functions.
Only a small part of the standard python modules are supported, but additional support can be easily added using :ref:`Python stubs <python-stubs>`.

There are currently two configurations available for the Python analysis:

- a type analysis [ECOOP20]_, called using ``mopsa-python-types``, whose configuration is defined in :config:`python/types.json`,

- a value analysis [SOAP20]_, called using ``mopsa-python``, whose configuration is defined in :config:`python/values.json`.


Options
-------

.. program:: python.flows.exceptions

.. option:: -unprecise-exn <exn1>,...,<exnn>

   All the raised exceptions the name of which are in the list will be collapsed into one environment (default: empty).

   This is useful to improve performance.
   This is particularly the case when the analysis is imprecise and, as a consequence, a large number of spurious reported exceptions of some kind is expected (e.g., all list accesses generating an ``IndexError`` exception when lists are abstracted with a smashing domain).

Allocation policy options
~~~~~~~~~~~~~~~~~~~~~~~~~

The Python analysis uses the recency abstraction.
As mentioned in :ref:`heap recency domain <recency>`, the default allocation policy is defined by the user.
In addition, this allocation policy can be refined for some specific builtin Python objects (currently, `list, range, dict, slice, tuple, set`).
An exemple of the benefits of configurability is shown in [SOAP20]_, Section 3.

.. program:: python.objects.dict

.. option:: -py-dict-alloc-pol (all | range | callstack | range_callstack)

   Allocation policy used for smashed dictionaries (default: ``all``).

.. program:: python.objects.list

.. option:: -py-list-alloc-pol (all | range | callstack | range_callstack)

   Allocation policy used for smashed lists (default: ``all``).

.. program:: python.objects.range

.. option:: -py-range-alloc-pol (all | range | callstack | range_callstack)

   Allocation policy used for range objects (default: ``all``).

.. option:: -py-slice-alloc-pol (all | range | callstack | range_callstack)

   Allocation policy used for slice objects (default: ``all``).

.. program:: python.objects.set

.. option:: -py-set-alloc-pol (all | range | callstack | range_callstack)

   Allocation policy used for smashed sets (default: ``all``).

.. program:: python.objects.tuple

.. option:: -py-tuple-alloc-pol (all | range | callstack | range_callstack)

    Allocation policy used for expanded tuples (default: ``all``).

Loops
~~~~~

.. program:: python.desugar.loops"

.. option:: -py-disable-desugar-for-range

   Disable the special desugaring on for-range-based loops.

   Desugaring is not worth it when no numerical domains are available.


Bibliographic References
------------------------

.. [ECOOP20] Raphaël Monat, Abdelraouf Ouadjaout, Antoine Miné: `Static Type Analysis by Abstract Interpretation of Python Programs. <https://www-apr.lip6.fr/~mine/publi/article-monat-al-ecoop20.pdf>`_ ECOOP 2020: 17:1–17:29
.. [SOAP20] Raphaël Monat, Abdelraouf Ouadjaout, Antoine Miné: `Value and Allocation Sensitivity in Static Python Analyses. <https://www-apr.lip6.fr/~mine/publi/article-monat-al-soap20.pdf>`_ SOAP\@PLDI 2020: 8–13
