==========================
**Mopsa** Developer Manuel
==========================

.. MOPSA developer manuel master file

Goal of the tutorial
====================

By opposition to the **Mopsa** user manual, which shows how to use the
different features already provided by the **Mopsa** static analyzer,
the goal of this tutorial is to present how new features can be
programmed in the **Mopsa** static analyzer.

Content of the tutorial
=======================

The **Mopsa** static analyzer is designed around the following idea: a
static analyzer defined by abstract interpretation can be defined as the
composition of small components (in our case these will be abstract
domains). The main advantages of defining an analyzer in this fashion
are :

-  The analysis is highly configurable (domains can be easily plugged in
   and out);
-  Each domain only has to focus on the features it is designed for,
   leaving the rest to other domains ; With that in mind, the main task

for a developer wanting to enrich the **Mopsa** static analyzer will be
the definition of abstract domains. Of course in order to achieve this
level of modularity while keeping precision, the different abstract
domain assembled in an analysis need to be able to collaborate. Several
collaboration mechanisms have been built inside **Mopsa**.

Moreover the programming language that **Mopsa** can analyze is not
permanently fixed. At this point, **Mopsa** can handle several real
world languages : C, Python, an imperative toy-language designed for
easiness of testing abstract domains and a stub language for the C
language. The second task for a developer wanting to enrich the scope of
the **Mopsa** static analyzer will therefore be the addition of features
to an existing language or the addition of an all together new language.

For these reasons this tutorial will start from the version of **Mopsa**
that can be found in opam or on the gitlab of the project and it will
walk the developer through the several ways in which language features
and new domains can be added to **Mopsa** while emphasizing on how to
make them collaborate efficiently and precisely and how they can be
assembled in a complex analyzer.

How to read this tutorial
=========================

Instead of emphasizing on which features are added in which sections of
the tutorial, we will underline the mechanisms of **Mopsa** at play in
each sections. If you are looking for something specific you will be
able to jump directly to the section targeting this mechanism in
**Mopsa**. For a comprehensive understanding of **Mopsa** we advise
reading through all sections of this tutorial (once you have gone
through the user manual).

.. toctree::
   :hidden:
   :caption: Introduction

   intro/architecture
   intro/mopsa-lib
   intro/configure
   intro/universal
   intro/starting-point

.. toctree::
   :hidden:
   :caption: Value Domain : The Sign abstract domain

   value-domain/introduction
   value-domain/sign
   value-domain/implementation
   value-domain/testing

.. toctree::
   :hidden:
   :caption: Relational Domain : The equalities domain

   equalities-domain/domains
   equalities-domain/managers
   equalities-domain/equalities-domain
   equalities-domain/implementation
   equalities-domain/queries
   equalities-domain/testing1
   equalities-domain/implementation2
