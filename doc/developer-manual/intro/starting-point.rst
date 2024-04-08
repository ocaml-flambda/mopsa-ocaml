==============
Starting point
==============

.. MOPSA developer manuel file

As mentioned we will use this toy-language to experiment with features
that we add to the **Mopsa** analyzer. Developing an analyzer from
scratch for this language would be of small interest in this tutorial as
this would be repetitive and cumbersome. Moreover it would involve
rebuilding some of the features provided by the framework. However we
provide in this section an overview of the existing features of
**Mopsa** that will be used out-of-the-box in the complete analysis we
will build in the first part of this tutorial.

Lexer / Parser
==============

In ``parsers/universal`` you will find a parser and a lexer for the
Universal language. The parser builds elements which types are defined
in ``parsers/universal/src/U_ast.ml``. These definitions provide the
notions of programs, statements, expressions and function declarations.
As shown in the previous section a program is composed of a list of
function declarations, a list of global variables and a list of
statements to be executed.

Extension of the AST of **Mopsa**
=================================

**Mopsa** always work on the same AST no matter its configuration,
therefore the AST of the language we want to analyze needs to be
translated into this common AST. **Mopsa** aims at being able to handle
several languages, without the need of a static translation from the
input language to some fixed common language (as this may require
precision-losing transformations). Therefore the core AST of **Mopsa**
on which the analyzer runs need to be extensible, so that any input
language may be injected into it, once extended. **Mopsa** defines types
that are common to every AST (representing e.g. expression, statements,
programs, constants, types, …), these are defined as empty types in the
core of **Mopsa** and are extended by the addition of new languages.
Therefore in file ``analyzer/src/lang/universal/ast.ml``, you will find
type definitions relative to the universal language as well as type
extensions.

For exemple the type ``typ`` (representing type expressions in **Mopsa**
AST) is extending in the following fashion :

.. literalinclude:: ./code/extension_example.ml
  :language: ocaml
  :linenos:
  :lines: 1-10

the type ``expr_kind`` (representing the kind of an expression in
**Mopsa** AST) is extending in the following fashion :

.. literalinclude:: ./code/extension_example.ml
  :language: ocaml
  :linenos:
  :lines: 11-20

the type ``prog_kind`` (representing the kind of programs in **Mopsa**
AST) is extending in the following fashion :

.. literalinclude:: ./code/extension_example.ml
  :language: ocaml
  :linenos:
  :lines: 22-27

These are only examples as the complete AST from
``parsers/universal/src/U_ast.ml`` must be injected into **Mopsa** AST.

Frontend
========

The AST produced by the parser then need to be translated into the newly
extended AST of **Mopsa**, this is done in
``analyzer/src/lang/universal/frontend.ml``. This is mainly a one-to-one
translation.

.. raw:: org

   #+LABEL: intro_starting-point_first-iterators

First iterators
===============

Now that we are able to parse, and translate the source into the
**Mopsa**, the analyzer can start analyzing the resulting program. The
idea behind **Mopsa** analysis is that handling of the semantics of
programs, statements, expressions, … is dispatched among several small
components called abstract domains. These domains only have to handle
part the overall features of the language leaving the rest for other
abstract domains to handle. Abstract domains in **Mopsa** can be
combined (we will see later how) to form new abstract domains, at the
start of an analysis all the abstract domains specified by a user are
combined into an abstract domain able to handle all the features of the
targeted language. This can be seen as assembling a pattern matching
broken down into several files into one big patter matching.

In order to analyze the universal language we therefore need a domain
describing how a ``P_universal`` program should be analyzed. This is the
role of the domain defined in
``analyzer/src/lang/universal/iterators/program.ml``:

.. literalinclude:: ./code/program.ml
  :language: ocaml
  :linenos:
  :emphasize-lines: 22-24

In this file we can find a module declaration ``Domain``, this domain
does not handle much of the features of the Universal language (e.g. it
answers ``None`` to every evaluation request as seen on line 16), except
for the fact that it describes how a universal program should be
analyzed : by analyzing the statements at the top-level of the program.

Similarly several other domains are defined handling some of the more
basic constructions of the universal language : loops, sequences of
statements, branching statements. We will start our tutorial using all
these domains so that for each abstract domain we design we only have to
focus on the handling of constructions related to our domain.

.. raw:: org

   #+LABEL: intro_starting-point_initial-configuration-file

Initial configuration file
==========================

Below is the default configuration file for **Mopsa** from which we will
start, using the already defined domains aforementioned. New domains
will be added in place of the highlighted line.

.. literalinclude:: ./code/base_config.json
  :language: js
  :linenos:
  :emphasize-lines: 11
