==============
Implementation
==============

.. MOPSA developer manuel

Domains in **Mopsa** are OCaml modules, the user can then choose at the
start of an analysis which of the domains he wants to use. In order for
**Mopsa** to be easily extensible, the list of available domains can not
be statically known, and should therefore be dynamically registered by
the user before any analysis is started (and before the configuration
file is read).

The registration function for a value domain can be found in module
``Mopsa.Sig.Value``, it takes as argument a module with signature
``Mopsa.Sig.Value.VALUE``.

From these remarks the global architecture of our implementation of the
sign domain, should look something like this :

.. literalinclude:: ./code/arch.ml
  :language: ocaml
  :linenos:

The signature ``VALUE`` can be split into 3 categories :

-  Definition of the lattice structure
-  Identification of the value domain
-  Definition of the abstract transformers

.. raw:: org

   #+LABEL: value-domain-lattice-operators

Lattice operators
=================

``VALUE`` signature excerpt :

.. literalinclude:: ./code/sig.ml
  :language: ocaml
  :linenos:
  :lines: 1-31

The lattice structure required by the ``VALUE`` signature is classical:

-  Definition of the type of the elements of the lattice
-  Definition of the set operators
-  Definition of some of the constants of the lattice

The type definition is quite straightforward as it is an enumeration of
the elements composing the lattice. In order to avoid numerous long
pattern matchings we defined a translation function from the sign
lattice to boolean triples and its inverse. The first boolean indicates
whether the abstract element represents at least one negative number,
the second whether it represents zero and the last whether it represents
a positive number.

No widening operator is needed as the lattice is of finite height.

.. literalinclude:: ./code/sign.ml
  :language: ocaml
  :linenos:
  :lines: 8-79

.. note::

   As this is the first example of abstraction definition in **Mopsa**
   we refrain from using utility domain builders provided by **Mopsa**
   in order to not introduce many notions at once. Note however that
   here we could have used :

   -  The ``Bot_top`` library which extends a type ``t`` by adding two
      elements :math:`\top` and :math:`\bot`.
   -  The ``Completed_partitioning`` library which transforms a finite
      partitioning :math:`P` of :math:`\mathbb I` into its completed
      lattice.

.. raw:: org

   #+LABEL: value-domain-domain-identification

Domain identification
=====================

``VALUE`` signature excerpt :

.. literalinclude:: ./code/sig.ml
  :language: ocaml
  :linenos:
  :lines: 32-42

Abstract domains in **Mopsa** (which are OCaml modules) are dynamically
stored in a table at the start of the analysis and are looked up when
building the analyzer. As the description of the analyzer is made
through a configuration file in ``.json`` format, domains need to be
given a unique string identifiers (its name). In addition, domains can
provide a rendering name which is used for printing abstract states.

TODO: Talk about types?

This identification process should be made by including the
``GenValueId`` functor provided by the ``Mopsa`` module. Indeed this
identification functor will not only provide the three identification
values required by the domain but will also register a comparison
function for the ``t id`` type. More on that later.

.. literalinclude:: ./code/sign.ml
  :language: ocaml
  :linenos:
  :lines: 85-91

.. raw:: org

   #+LABEL: value-domain-abstract-transformers

Abstract transformers
=====================

According to the ``VALUE`` signature, we also need to define abstract
transformers operating on type ``t``. In ``Mopsa``, abstract
transformers are usually defined in terms of an ``exec`` and an ``eval``
function describing the semantics of statements and expressions.
However, in order to simplify the definition of a value domain, the
lifter (from value domain to environment abstraction) will define these
based on the few following abstract operators. Please note that instead
of having to deal with expressions and statements we only have to worry
about types, operators and constants.

.. literalinclude:: ./code/sig.ml
  :language: ocaml
  :linenos:
  :lines: 44-107

.. note::

   In addition to the classical operators, you can note here that the
   ``VALUE`` signature offers the opportunity of defining an ``ask``
   function. This function is used as a quick and easy way to transmit
   information between abstract domains wanting to collaborate without
   knowing each other. This is an important feature of **Mopsa** which
   will get its own section, for this reason we leave it empty here (by
   answering ``None`` to every question) and will come back on it in a
   future section.

The sign domain we are currently defining will try not only try to
represent integers, but also boolean (with the classical :math:`0` means
``true`` and anything non :math:`0` means ``false``). The ``typ``
argument can be used to separate the handling of boolean and integers
for increased precision.

.. literalinclude:: ./code/sign.ml
  :language: ocaml
  :linenos:
  :lines: 93-231

For concision, we left out the handling of backward operators and fell
back to the default ones provided by the framework. The definition of
the other operators is quite classical and straightforward and does not
use any **Mopsa** features.

The complete implementation can be found here:
:download:`signs.ml <./code/sign.ml>`

.. raw:: org

   #+LABEL: value-domain-compilation

Compilation
===========

The file ``signs.ml`` that we just defined can be stored anywhere within
``analyzer/src``, in order to keep with the existing file organization
we advise (and will assume) that it was saved in the
``analyzer/src/lang/universal/numeric/values/`` directory. To add this
file to the **Mopsa** analyzer and to *pack* it in the right module you
then have to edit the ``analyzer/src/lang/universal/Makefile`` file.
This file describes *packs* related to the universal language: each line
``x = a b c`` is the definition of an OCaml module named ``x``
containing the modules ``a``, ``b`` and ``c``. Therefore you should add
your newly defined module ``sign`` to the line
``lang.universal.numeric.values`` producing something like :

.. code:: bash

   lang.universal.numeric.values = intervals congruences zero powerset signs

We are now ready to test our newly defined domain.
