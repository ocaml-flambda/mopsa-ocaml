=======
Domains
=======

.. MOPSA developer manuel

The sign abstraction from the previous section lacked the ability to
express relations between the numeric variables of the program.

In the previous section we defined an abstract domain without having to
look at the actual signature that those should provide in **Mopsa** as
we relied on a predefined domain builder.

**Mopsa** provides several levels of signatures for abstract domains:

-  *stateless*
-  *simplified*
-  *standard*

We have seen (and will see again) examples of stateless domains (which
are usually called *iterators*). In this section we therefore focus on
the second level of domain: *simplified domains*.

In the **Mopsa** engine all domains defined by the programmer (be it
stateless, simplified or standard) are transformed into a standard
domain before being plugged in the analyzer. For this reason any domain
signature can be swapped with any other in the configuration file.

As for value domains, a domain should provide:

-  a lattice structure
-  identification
-  abstract transformers

In a value domain abstract transformers were defined in terms of
semantics of basic operators, however in a classical **Mopsa** domain,
the semantics is described in terms of an ``exec`` function. Even though
the different levels of abstract domains in **Mopsa** do not share the
same signature for the ``exec`` function, the idea is always the same:
``exec`` is given as input a statement of the **Mopsa** AST and an input
state and is asked to produce a sound post state. At this point let us
insist on two important points about the ``exec`` function of **Mopsa**:

#. The handling of statements from the input language should be
   dispatched among several domains. Indeed each domain should target
   only a few of the features of the complete language. For this reason
   the loop iterator of **Mopsa** only handles ``S_while`` statements.
   Therefore the ``exec`` function of an abstract domain has the
   possibility of answering ``None`` when asked the post-condition of a
   statement. This will tell the framework that this domain does not
   know the semantics of this statement and it should ask to another
   domain (the next one in the configuration file).
#. It is often the case that the handling of a statement requires the
   collaboration of other abstract domains. Imagine for example the
   analysis of a ``if (e) {s1} else {s2}`` statement. The
   intra-procedural iterator handling this statement only knows that :

   .. math::

      \mathbb{S} [\![ \texttt{if (e) {s1} else {s2}} ]\!](S^{\sharp}) =
        \mathbb{S} [\![ \texttt{s1} ]\!] \circ \mathbb{S} [\![ \texttt{e}?
        ]\!](S^{\sharp}) \sqcup \mathbb{S} [\![ \texttt{s2} ]\!] \circ \mathbb{S}
        [\![ \texttt{!e}?
        ]\!](S^{\sharp})

   However it does not necessarily know how to compute
   :math:`\mathbb{S} [\![\texttt{s1} ]\!]`,
   :math:`\mathbb{S} [\![ \texttt{s2} ]\!]`,
   :math:`\mathbb{S} [\![ \texttt{e}? ]\!]` or
   :math:`\mathbb{S} [\![\texttt{!e}? ]\!]`. Therefore this domain need
   to be able to ask the rest of the abstract domains whether or not
   they know any of this post-conditions. Keep in mind that domains do
   not know with which domains they have been assembled in this
   analysis. For this reason, in addition to been given a statement and
   an input states, the ``exec`` function will also be given a so-called
   *manager* which can be used to ask the overall analyzer to execute
   statements. It is easy to see here that this is not the only use of
   this manager. Indeed the :math:`\sqcup` operator in the previous
   formula is also given to the intra-procedural iterator inside this
   manager.

.. note::

   As domains need to be able to manipulate the entire abstraction
   without knowing precisely what composes this entire abstraction.
   Therefore it will often be the case that functions manipulates
   universally quantified types denoting the type of the overall
   abstraction. It is a convention in **Mopsa** that type ``'a`` denotes
   the type of the complete abstract state.
