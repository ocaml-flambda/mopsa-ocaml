============================
Testing (1), domains product
============================

.. MOPSA developer manuel

.. raw:: org

   #+LABEL: equalities-domain_testing1

Let us add our newly defined equalities domain to the existing
configuration. Both the sign domain and the equality domain abstracts
set of numerical environment. They do not target the same properties,
therefore we would like them to be assembled in a product.

Configuration file
==================

In the configuration file, domains can be composed in a product
(yielding a new abstract domain) using the key-word ``product``. as in
the following configuration file:
:download:`sign_simple-eq.json <./code/sign_simple-eq.json>`

.. literalinclude:: ./code/sign_simple-eq.json
  :language: js
  :linenos:
