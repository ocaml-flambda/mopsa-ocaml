==================
Implementation (1)
==================

.. MOPSA developer manuel

.. raw:: org

   #+LABEL: equalities-domain_implementation1

The *simplified* signature
==========================

The *simplified* signature is described in the type signature
``Mopsa.Sig.Abstraction.Simplified.SIMPLIFIED``. Moreover, once again,
the domain that we will define here should be registered as a simplified
domain. This can be done using the
``Mopsa.Sig.Abstraction.Simplified.register_simplified_domain``
function. The architecture of our implementation should therefore look
something like this:

.. literalinclude:: ./code/arch_impl1.ml
  :language: ocaml
  :linenos:

.. note::

   The ``SIMPLIFIED`` signatures provides only a simplified manager to
   its transfer functions. This manager only provides simplified version
   of the ``exec`` and ``ask`` functions. However in this signature
   level all transfer functions should operate at the domain level and
   not at the analyzer level (meaning that operators are defined over
   the type ``t`` of the domain, instead of the global type ``'a`` of
   the complete abstract state), this justify why we do not need the
   ``get`` and ``set`` functions shown in the
   :ref:`Managers section <equalities-domain_managers>`

.. raw:: org

   #+LABEL: equalities-domain-lattice-operators

Lattice operators
=================

``SIMPLIFIED`` signature excerpt :

.. literalinclude:: ./code/sig.ml
  :language: ocaml
  :linenos:
  :lines: 1-43

This part of the signature is very similar to the one required by a
value domain, with the main different being the ``merge`` function. The
merging mechanism of **Mopsa** (what it is and why it is necessary)
requires an in-depth presentation that we post-pone for now.

The Hasse diagram of
:ref:`Equalities domain <relational-domain_equalities-domain>` shows
that the type of the element of the lattice should be sets of sets of
variables or :math:`\bot`. This can be easily defined using the ``Bot``
modules provided by the ``Utils`` library shipped with ``Mopsa``. Indeed
the type ``t
Bot.with_bot`` represents :math:`\texttt{t} \cup \{\bot\}`.

As per the presentation of the equalities domain, we start by defining a
``wf`` operator taking as input an abstract element and producing its
normalization. Abstract union is easily defined by remarking that the
most precise join of two partitions (denoting an equality class) is the
intersection of the two classes
(:math:`(x = y = z) \vee (y = z = t) \Rightarrow (y = z)`), and by
expanding the union to every pair of equality classes.

The set of partitions has an intersection semantics, therefore the meet
can easily be computed by uniting the two sets of equivalence classes
and normalizing the result.

Finally the inclusion test :math:`S_1^{\sharp} \subseteq S_2^{\sharp}`
can be computed by testing the every equality class from
:math:`S_2^{\sharp}` is implied by one of the equality class from
:math:`S_1^{\sharp}`.

.. literalinclude:: ./code/impl1.ml
  :language: ocaml
  :linenos:
  :lines: 13-91

.. raw:: org

   #+LABEL: equalities-domain-domain-identification

Domain identification
=====================

``SIMPLIFIED`` signature excerpt :

.. literalinclude:: ./code/sig.ml
  :language: ocaml
  :linenos:
  :lines: 44-50

Domain identification can be made using a identification functor
provided by the ``Id`` module (which is included in ``Mopsa``) :

.. literalinclude:: ./code/impl1.ml
  :language: ocaml
  :linenos:
  :lines: 93-101

.. raw:: org

   #+LABEL: equalities-domain-abstract-transformers

Abstract Transformers
=====================

``SIMPLIFIED`` signature excerpt :

.. literalinclude:: ./code/sig.ml
  :language: ocaml
  :linenos:
  :lines: 52-61

This *simplified* domain signature requires the definition of 3 transfer
functions :

-  ``init``: this function describes how the domain should be
   initialized at the start of the program. In our case we can start
   from ``top``.
-  ``exec``: as mentioned, this function extends the global pattern
   matching of program instructions. When defining an ``exec`` function,
   one should always keep in mind: *What is the bare minimum program
   statements my domain should handle?* In our case we are defining a
   numerical domain, abstracting set of environments, therefore we
   should only handle instructions related to sets of environments
   manipulations:

   -  ``S_add of var`` is the addition of an unconstrained variable;
   -  ``S_remove of var`` is the removal of a variable;
   -  ``S_forget of var`` is the forgetting of a variable (the variable
      should still be present if it was but unconstrained);
   -  ``S_rename of var * var`` is the renaming of a variable;
   -  ``S_project of var list`` is the projection to the specified list
      of variable;
   -  ``S_assign of var * expr`` denotes the affectation of an
      expression to a variable;
   -  ``S_expand of var * var list`` denotes the expansion of the first
      variable into the second list of variables;
   -  ``S_fold of var * var list`` is the dual operator that folds the
      second list of variables in the first variables;
   -  ``S_assume of expr`` filters to keep only environments satisfying
      ``expr``.

-  ``ask``: this function is used for quick access to information from
   other domain. We do not provide the implementation here as the next
   manual page will be entirely devoted to this query mechanism.

.. literalinclude:: ./code/impl1.ml
  :language: ocaml
  :linenos:
  :lines: 103-232

Note that in the case of our simple equality domain we could define all
transfer functions using only:

-  the instruction to execute ``stmt``
-  the input state ``env``

The signature does not ask of us to return an element of type ``t`` but
of type ``t
option``, leaving us the possibility to answer ``None`` when asked for a
postcondition. This tells the engine that we are not in charge of such
statements and it should ask to some other domains.
