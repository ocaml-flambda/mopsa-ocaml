=====================
Testing and debugging
=====================

.. MOPSA developer manuel

.. note::

   As you improve and test new abstract domains in **Mops**, you will be
   faced with potentially long compilation times, we recall that you can
   disable ``C`` and ``Python`` domains with the configure options
   ``--disable-c`` and ``--disable-python``.

Configuration file
==================

Building on top of our
:ref:`starting configuration file <intro_starting-point_initial-configuration-file>`,
we can now define a new configuration file that uses not only the
already defined stateless domain handling some of the semantics of the
universal domain, but also the sign domain. We named our sign domain
``"universal.numeric.values.sign"`` however it is only a value
abstraction and not an abstract domain, meaning that it can not be used
as is in a configuration file describing a composition of abstract
domains. We therefore have to specify that this value domain should be
lifted to an abstract domain using the ``nonrel`` lifter. This can be
done using the "nonrel" key (recall that configuration files are written
with a ``.json`` syntax):

``{"nonrel": "universal.numeric.values.sign"}``

The complete configuration file should look like this :
:download:`sign.json <./code/sign.json>`.

.. literalinclude:: ./code/sign.json
  :language: js
  :linenos:

First analysis and debugging tools
==================================

We are now ready to start our first analysis. Let us write a small test
file: :download:`first.u <./code/tests/first.u>`

.. literalinclude:: ./code/tests/first.u
  :language: c
  :linenos:

Launching **Mopsa** on this test file should produce the following
output :

.. raw:: html

   <div style="font-family: inconsolata,monospace; background:#343131; color:white; border:1px solid black; padding: 5px">
   $ ./bin/mopsa -config=share/mopsa/configs/universal/sign.json analyzer/tests/universal/manual/signs/first.u<br>
   <span style="font-weight: bold; color: #00a800">Analysis terminated successfully</span><br>
   <span style="font-weight: bold; color: #00a800">✔</span> No alarm<br>
   Time: 0.001s<br>
   <br>
   </div>

While this is reassuring, this does not provide a lot of information.
Let us have a look at the debugging tools provided by the framework :

.. note::

   We do not recall here how the interactive mode of **Mopsa** works and
   refer the reader to the **Mopsa** user manual for more information.

Instrumenting the source code
-----------------------------

The Universal language comes with builtin functions that enabling the
user to modify or print the analyzer state for debugging purposes.

Printing the analyzer state
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The function ``print`` can be called anywhere in the source code, it
does modify the current state and tells the analyzer to print the
current state. For example, adding a ``print()`` statement at the end of
our starting example we get:
(:download:`first_print.u <./code/tests/first_print.u>`)

.. literalinclude:: ./code/tests/first_print.u
  :language: c
  :linenos:
  :emphasize-lines: 6

Launching **Mopsa** on this test file should produce the following
output :

.. raw:: html

   <div style="font-family: inconsolata,monospace; background:#343131; color:white; border:1px solid black; padding: 5px">
   $ ./bin/mopsa -config=share/mopsa/configs/universal/sign.json analyzer/tests/universal/manual/signs/first_print.u<br>
   analyzer/tests/universal/manual/signs/first_print.u:6.0-8<br>
     ⏵ cur ↦<br>
         sign:   x ⇀ &gt;0,<br>
                 y ⇀ &lt;0,<br>
                 z ⇀ ⊤<br>
         <br>
     |alarms| = 0<br>
   <span style="font-weight: bold; color: #00a800">Analysis terminated successfully</span><br>
   <span style="font-weight: bold; color: #00a800">✔</span> No alarm<br>
   Time: 0.001s<br>
   <br>
   </div>

We can now see that the abstract state consists only of the lifting of
the sign domain and that it successfully discovered that ``x`` is
greater than :math:`0` and ``y`` less than :math:`0`.

Adding hypotheses to the analyzer state
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For test purposes, it is often the case that one need the abstract state
to be in a state that can not be easily reached by using the abstract
semantics of the target language. For example here we would like ``z``
to be non negative.

This can be achieved using the builtin function ``assume``. A call
``assume(e)`` where ``e`` is an expression of the universal language
will be translated into an ``Assume(e)`` expression in the **Mopsa**
AST.

Continuing our toy example we get :
(:download:`first_print_assume.u <./code/tests/first_print_assume.u>`)

.. literalinclude:: ./code/tests/first_print_assume.u
  :language: c
  :linenos:
  :emphasize-lines: 6

Launching **Mopsa** on this test file should produce the following
output :

.. raw:: html

   <div style="font-family: inconsolata,monospace; background:#343131; color:white; border:1px solid black; padding: 5px">
   $ ./bin/mopsa -config=share/mopsa/configs/universal/sign.json analyzer/tests/universal/manual/signs/first_print_assume.u<br>
   analyzer/tests/universal/manual/signs/first_print_assume.u:7.0-8<br>
     ⏵ cur ↦<br>
         sign:   x ⇀ &gt;0,<br>
                 y ⇀ &lt;0,<br>
                 z ⇀ ≥0<br>
         <br>
     |alarms| = 0<br>
   analyzer/tests/universal/manual/signs/first_print_assume.u:7.0-8<br>
     ⊥<br>
     |alarms| = 0<br>
   <span style="font-weight: bold; color: #00a800">Analysis terminated successfully</span><br>
   <span style="font-weight: bold; color: #00a800">✔</span> No alarm<br>
   Time: 0.001s<br>
   <br>
   </div>

Testing the precision of the analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally it is possible to test the precision of the analysis by
asserting that some expressions should be evaluated to true. In our case
if we wanted to verify that the multiplication of a positive and
negative number has been correctly implemented in the sign domain, we
could modify our small example in the following way :

(:download:`first_print_assume_assert.u <./code/tests/first_print_assume_assert.u>`)

.. literalinclude:: ./code/tests/first_print_assume_assert.u
  :language: c
  :linenos:
  :emphasize-lines: 7

Launching **Mopsa** on this test file should produce the following
output :

.. raw:: html

   <div style="font-family: inconsolata,monospace; background:#343131; color:white; border:1px solid black; padding: 5px">
   $ ./bin/mopsa -config=share/mopsa/configs/universal/sign.json analyzer/tests/universal/manual/signs/first_print_assume_assert.u<br>
   analyzer/tests/universal/manual/signs/first_print_assume_assert.u:8.0-8<br>
     ⏵ cur ↦<br>
         sign:   x ⇀ &gt;0,<br>
                 y ⇀ &lt;0,<br>
                 z ⇀ ≥0<br>
         <br>
     |alarms| = 0<br>
   Analysis terminated successfully<br>
   ✔ No alarm<br>
   Time: 0.001s<br>
   <br>
   </div>

Let us assume now and in the following section that our multiplication
operator was poorly implemented :

.. literalinclude:: ./code/bad.ml
  :language: ocaml
  :linenos:
  :emphasize-lines: 5

Launching **Mopsa** would then produce the following :

.. raw:: html

   <div style="font-family: inconsolata,monospace; background:#343131; color:white; border:1px solid black; padding: 5px">
   $ ./bin/mopsa -config=share/mopsa/configs/universal/sign.json analyzer/tests/universal/manual/signs/first_print_assume_assert.u<br>
   analyzer/tests/universal/manual/signs/first_print_assume_assert.u:8.0-8<br>
     ⏵ cur ↦<br>
         sign:   x ⇀ &gt;0,<br>
                 y ⇀ &lt;0,<br>
                 z ⇀ ≥0<br>
         <br>
     |alarms| = 0<br>
   analyzer/tests/universal/manual/signs/first_print_assume_assert.u:8.0-8<br>
     ⊥<br>
     |alarms| = 1<br>
   analyzer/tests/universal/manual/signs/first_print_assume_assert.u:8.0-8<br>
     ⊥<br>
     |alarms| = 0<br>
   <span style="font-weight: bold; color: #00a800">Analysis terminated successfully</span><br>
   <br>
   <span style="font-weight: bold">analyzer/tests/universal/manual/signs/first_print_assume_assert.u</span>: In function '<span style="font-weight: bold">&lt;&gt;</span>':<br>
   <span style="font-weight: bold">analyzer/tests/universal/manual/signs/first_print_assume_assert.u:7.7-16</span>: <span style="font-weight: bold; color: #E850A8">Assertion fail</span><br>
     <br>
     <span style="font-weight: bold">7</span>: assert(<span style="font-weight: bold; color: #ff0000">x * y &lt; 0</span>);<br>
               <span style="font-weight: bold; color: #ff0000">^^^^^^^^^</span>  <br>
     Assertion '<span style="font-weight: bold">((x * y) &lt; 0)</span>' violated<br>
   <br>
   Summary of detected alarms:<br>
     Assertion fail: 1<br>
     Total: 1<br>
   <br>
   Time: 0.001s<br>
   <br>
   </div>

Instrumenting the abstract domain
---------------------------------

We use the opportunity of this presentation of debugging tools to
present the ``Debug`` module provided by **Mopsa**. The ``Debug`` module
provides a ``debug`` function taking as optional argument a channel name
and producing a formatting function. Channels can easily be turned on
and off from the command line using the ``-channel`` option. In order to
tune in to several channels, these should be comma separated and ``_``
can be used as a wildcard.

For example here we could add the following to the definition of our
multiplication operator :

.. code:: ocaml

   Debug.debug ~channel:"sign" "%a * %a = %a" print x print y print rep

this results in :

.. raw:: html

   <div style="font-family: inconsolata,monospace; background:#343131; color:white; border:1px solid black; padding: 5px">
   $ ./bin/mopsa -config=share/mopsa/configs/universal/sign.json analyzer/tests/universal/manual/signs/first_print_assume_assert.u -debug=sign<br>
   <div style="background:darkgray; border:1px solid black; padding: 5px">
   <span style="font-weight: bold; color: #a8542a">[sign 0.004]</span> &gt;0 * &lt;0 = ≤0<br>
   <span style="font-weight: bold; color: #a8542a">[sign 0.004]</span> &gt;0 * &lt;0 = ≤0<br>
   </div>
   analyzer/tests/universal/manual/signs/first_print_assume_assert.u:8.0-8<br>
     ⏵ cur ↦<br>
         sign:   x ⇀ &gt;0,<br>
                 y ⇀ &lt;0,<br>
                 z ⇀ ≥0<br>
         <br>
     |alarms| = 0<br>
   analyzer/tests/universal/manual/signs/first_print_assume_assert.u:8.0-8<br>
     ⊥<br>
     |alarms| = 1<br>
   analyzer/tests/universal/manual/signs/first_print_assume_assert.u:8.0-8<br>
     ⊥<br>
     |alarms| = 0<br>
   <span style="font-weight: bold; color: #00a800">Analysis terminated successfully</span><br>
   <br>
   <span style="font-weight: bold">analyzer/tests/universal/manual/signs/first_print_assume_assert.u</span>: In function '<span style="font-weight: bold">&lt;&gt;</span>':<br>
   <span style="font-weight: bold">analyzer/tests/universal/manual/signs/first_print_assume_assert.u:7.7-16</span>: <span style="font-weight: bold; color: #E850A8">Assertion fail</span><br>
     <br>
     <span style="font-weight: bold">7</span>: assert(<span style="font-weight: bold; color: #ff0000">x * y &lt; 0</span>);<br>
               <span style="font-weight: bold; color: #ff0000">^^^^^^^^^</span>  <br>
     Assertion '<span style="font-weight: bold">((x * y) &lt; 0)</span>' violated<br>
   <br>
   Summary of detected alarms:<br>
     Assertion fail: 1<br>
     Total: 1<br>
   <br>
   Time: 0.001s<br>
   <br>
   </div>

where we can clearly see our mistake.

Unit testing
------------

Finally **Mopsa** provides a unit test mode. In Unit test mode, the
classical program iterator (seen in
:ref:`First iterators <intro_starting-point_first-iterators>` is
replaced by an iterator that will execute the body of each function
which name starts with ``test``.

Factorial
=========

Consider now the following program computing ``n!`` in ``y``.
(:download:`fact0.u <./code/tests/fact0.u>`):

.. literalinclude:: ./code/tests/fact0.u
  :language: c
  :linenos:

The analysis using the sign domain yields :

.. raw:: html

   <div style="font-family: inconsolata,monospace; background:#343131; color:white; border:1px solid black; padding: 5px">
   $ ./bin/mopsa -config=share/mopsa/configs/universal/sign.json analyzer/tests/universal/manual/signs/fact0.u<br>
   <span style="font-weight: bold; color: #00a800">Analysis terminated successfully</span><br>
   <span style="font-weight: bold; color: #00a800">✔</span> No alarm<br>
   Time: 0.001s<br>
   <br>
   </div>

The analysis therefore able to prove that this program always compute a
positive value for a factorial.

However on the following program (which also computes ``n!`` in ``y)``
(:download:`fact1.u <./code/tests/fact1.u>`):

.. literalinclude:: ./code/tests/fact1.u
  :language: c
  :linenos:

The analysis fails as ``n`` is decreased by 1 which might result in it
becoming negative:

.. raw:: html

   <div style="font-family: inconsolata,monospace; background:#343131; color:white; border:1px solid black; padding: 5px">
   $ ./bin/mopsa -config=share/mopsa/configs/universal/sign.json analyzer/tests/universal/manual/signs/fact1.u<br>
   <span style="font-weight: bold; color: #00a800">Analysis terminated successfully</span><br>
   <br>
   <span style="font-weight: bold">analyzer/tests/universal/manual/signs/fact1.u</span>: In function '<span style="font-weight: bold">&lt;&gt;</span>':<br>
   <span style="font-weight: bold">analyzer/tests/universal/manual/signs/fact1.u:8.7-10</span>: <span style="font-weight: bold; color: #E850A8">Assertion fail</span><br>
     <br>
     <span style="font-weight: bold">8</span>: assert(<span style="font-weight: bold; color: #ff0000">y&gt;0</span>);<br>
               <span style="font-weight: bold; color: #ff0000">^^^</span>  <br>
     Assertion '<span style="font-weight: bold">(y &gt; 0)</span>' violated<br>
   <br>
   Summary of detected alarms:<br>
     Assertion fail: 1<br>
     Total: 1<br>
   <br>
   Time: 0.003s<br>
   <br>
   </div>
