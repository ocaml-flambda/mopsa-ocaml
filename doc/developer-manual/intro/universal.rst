==========================
The ``Universal`` language
==========================

.. MOPSA developer manuel file

``Universal`` is a small imperative ``C``-like toy language. Having such
a language in **Mopsa** has two merits :

-  Firstly it provides a simple language on which users may test their
   newly added features (this is what we will do in the start of the
   tutorial);
-  Secondly it allows the factorization of domain definitions, indeed
   the semantics of a ``while`` loop in ``C`` or ``Python`` are close
   from each other. Therefore assuming that we have already defined a
   domain handling the semantics of a ``while`` loop in universal, we
   will be able to reuse the domain for both ``C`` and ``Python``
   analysis.

Syntax of ``Universal``
=======================

Universal has the following syntax:

.. code:: text

   <program>   ::= <declaration>* <function>* <block>

   <declaration>   ::= <type> <id>;

   <type>      ::= "int" | "real" | <type> "array"

   <function>  ::= <type> <id>(<type> <id>, ...) { <block> };

   <block>     ::= <stmt>*

   <stmt>      ::= <lval> = <expr>;
            |  "if" (<expr>) <block>
            |  "if" (<expr>) <block> "else" <block>
            |  "while" (<expr>) <block>
            |  "break";
            |  "continue";
            |  "return" <expr>;
            |  "print" ();

   <expr>       ::= <int> | <real> | <id>
            | <unop> <expr>
            | <expr> <binop> <expr>
            | <expr>[<expr>]
            | "rand"(<int>, <int>)
            | <id>(<expr>, ...)

   <unop>       ::= "+" | "-" | "!"

   <binop>      ::= "+" | "-" | "*" | "/" | "<" | "==" | "!=" | "<" | "<=" | ">" | ">="

Example
=======

The following example, illustrates a ``Universal`` program computing the
remainder of the Euclidean division of two integer variables:
:download:`remainder.u <code/remainder.u>`

.. literalinclude:: ./code/remainder.u
  :language: c
  :linenos:
