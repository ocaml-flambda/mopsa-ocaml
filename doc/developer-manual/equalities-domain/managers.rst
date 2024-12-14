========
Managers
========

.. MOPSA developer manuel

.. raw:: org

   #+LABEL: equalities-domain_managers

There are several forms of *managers* in **Mopsa**. The idea behind a
manager is however always the same : To give access to parts of the
analyzer.

The classic scenario is the following: a domain :math:`\mathcal D`
defining an abstract state of type ``t`` is given access to the complete
state of the analyzer: ``'a`` and to a manager ``('a, t) man``. This
manager is a structure containing the following functions (some optional
arguments where removed from the signature for presentation purposes) :

.. literalinclude:: ./code/manager.ml
  :language: ocaml
  :linenos:
  :lines: 13-31

where ``'a lattice`` is a structure containing functions allowing
lattice manipulations over lattice elements of type ``'a``.

.. note::

   The ``man`` types mentions types not seen before (eg. ``'a flow``,
   ``'a post``), these can be thought of, for now, as the ``'a`` type :
   the abstract state of the complete analyzer.

Such an ``('a, t) man`` manager provides :

-  a way to read and write the information associated with domain
   :math:`\mathcal D` in the complete abstract state (which is composed
   of the abstract state of all domains assembled in the analyzer) via
   the ``get`` and ``set`` functions.
-  a way to perform lattice operations on the complete abstract state,
   even though domain :math:`\mathcal D` is unaware of its component.
-  a way to ask other domains to execute statements and evaluate
   expressions.

Of course different levels of domain signatures comes with different
levels of manager types.
