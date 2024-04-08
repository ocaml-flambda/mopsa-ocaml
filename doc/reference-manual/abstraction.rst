Abstraction
===========

The set of program environments are over-approximated by a computable abstraction.
For modularity reasons, this global abstraction is not defined by a single abstract domain.
Instead, it is decomposed into several abstract domains that handle different parts of the semantics of the target language.
Consequently, the global abstraction depends on the selected domains for a given analysis.
For this reason, transfer functions of abstract domains in Mopsa are *polymorphic*, *i.e.* they depend on a type variable ``'a`` representing the global abstraction.
Mopsa provides domains an API in order to manipulate values of type ``'a`` (*e.g.* performing unions and checking inclusions) and also to apply transfer functions (*e.g.* executing statements and evaluating expressions).

Lattice
-------

The global abstraction has a lattice structure with usual operators, encapsulated in the type ``'a lattice``, defined in `framework/core/lattice.ml <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/lattice.ml>`_:

.. code-block:: ocaml

   type 'a lattice = {
     bottom:    'a;
     top:       'a;
     is_bottom: 'a -> bool;
     subset:    'a ctx -> 'a -> 'a -> bool;
     join:      'a ctx -> 'a -> 'a -> 'a;
     meet:      'a ctx -> 'a -> 'a -> 'a;
     widen:     'a ctx -> 'a -> 'a -> 'a;
     print:     printer -> 'a -> unit;
  }


.. describe:: bottom : 'a

   The least element of the lattice.

.. describe:: top : 'a

   The greatest element of the lattice.

.. describe:: is_bottom : 'a -> true

   ``is_bottom a`` checks whether ``a`` is the least element of the lattice.

.. describe:: subset : 'a ctx -> 'a -> 'a -> bool

   Partial order relation. ``subset ctx a1 a2`` tests whether ``a1`` is related to (or included in) ``a2``. The parameter ``ctx`` is the flow-insensitive store, described :ref:`later<Context>`.

.. describe:: join : 'a ctx -> 'a -> 'a -> 'a

   ``join ctx a1 a2`` computes an upper bound of ``a1`` and ``a2``.

.. describe:: meet : 'a ctx -> 'a -> 'a -> 'a

   ``join ctx a1 a2`` computes a lower bound of ``a1`` and ``a2``.


.. describe:: widen : 'a ctx -> 'a -> 'a -> 'a

   ``widen ctx a1 a2`` computes an upper bound of ``a1`` and ``a2`` that ensures stabilization of ascending chains.

.. describe:: print : printer -> 'a -> unit

   Pretty-printer of abstract elements. See the :ref:`Print API<Print>`.


Control flows
-------------

The global abstraction is useful for representing the environments of the program.
In order to implement iterators, it is often necessary to represent environments from different control flows.
For example, a loop iterator that operates by induction on the syntax needs to maintain separate environments reaching ``break`` and ``continue`` statements.
Similarly to other analyzers, Mopsa represents such non-local control flows as *continuations* that store the environments suspended at specific program locations.


Tokens
^^^^^^

Each continuation is identified by a *token* of type ``token``, defined in `framework/core/token.ml <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/token.ml>`_.
The framework defines a builtin token ``T_cur`` representing environments that reach the current execution point.
Domains can extend this type to add new kinds of tokens representing particular control flows.
For example, the iterator for ``goto`` statements in C defines a new token ``T_goto of string`` to store environments suspended at a ``goto label;`` statement (see `lang/c/iterators/goto.ml <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/lang/c/iterators/goto.ml>`_):

.. code-block:: ocaml

   type token += T_goto of string (* label of the target *)

   register_token {
     print = (fun next fmt -> function
       | T_goto l -> Format.fprintf fmt "goto %s" l
       | tk       -> next fmt tk
     );
     compare = (fun next tk1 tk2 ->
       match tk1,tk2 with
       | T_goto x, T_goto y -> compare x y
       | _                  -> next a b
     )
   };;

When the program reaches the statement ``label: stmt;``, the iterator takes the abstract environment attached to token ``T_goto(label)``, and joins it to the environments at the ``T_cur`` token, before executing statement ``stmt``.

.. note::

   The set of tokens need to be finite.

The framework defines the usual API for extensible types:

.. describe:: val pp_token : Format.formatter -> token -> unit

   Print a token.

.. describe:: val compare_token : token -> token -> int

   Compare two tokens.

.. describe:: val register_token: token TypeExt.info -> unit

   Register a new token.


Flows
^^^^^

All reachable program environments, including those suspended at other program locations, are represented by a map ``'a flow`` binding tokens to abstract environments.
The type ``'a flow`` is defined in `framework/core/flow.ml <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/flow.ml>`_.
It has a lattice structure with the follwing API:

.. describe:: Flow.bottom: 'a flow

   The least element of the lattice of flows, representing the empty set of traces.

.. describe:: Flow.top : 'a ctx -> 'a flow

   The greatest element of the lattice of flows, representing all possible traces.

.. describe:: Flow.singleton: 'a ctx -> token -> 'a -> 'a flow

   Flow with a single binding.


.. describe:: Flow.is_bottom : 'a lattice -> 'a flow -> bool

   Emptiness test.

.. describe:: Flow.subset : 'a lattice -> 'a flow -> 'a flow -> bool

   Inclusion test.

.. describe:: Flow.join : 'a lattice -> 'a flow -> 'a flow -> 'a flow

   Upper bound of two flows.

.. describe:: join_list : 'a lattice -> empty:(unit -> 'a flow) -> 'a flow list -> 'a flow

   Upper bound of a list of flows. Parameter ``empty`` defines the flow to return if the list is empty.

.. describe:: Flow.meet : 'a lattice -> 'a flow -> 'a flow -> 'a flow

   Lower bound of two flows.

.. describe:: Flow.meet_list : 'a lattice -> empty:'a flow -> 'a flow list -> 'a flow

   Upper bound of a list of flows. Parameter ``empty`` defines the flow to return if the list is empty.

.. describe:: Flow.widen : 'a lattice -> 'a flow -> 'a flow -> 'a flow

   Upper bound of two flows that ensures stabilization of ascending chains.

.. describe:: Flow.print : (Print.printer -> 'a -> unit) -> Print.printer -> 'a flow -> unit

   ``Flow.print f p flow`` prints all bindings in ``flow`` with printer ``p``. Each environments is printed using function ``f``.

.. describe:: Flow.get : token -> 'a lattice -> 'a flow -> 'a

   ``Flow.get tk lattice flow`` returns the abstract element associated to token ``tk`` in ``flow``. Returns ``lattice.bottom`` if the binding is not found.

.. describe:: Flow.set : token -> 'a -> 'a lattice -> 'a flow -> 'a flow

   ``Flow.set tk a lattice flow`` overwrites the binding of token ``tk`` in ``flow`` with the abstract element ``a``.

.. describe:: Flow.add: token -> 'a -> 'a lattice -> 'a flow -> 'a flow

   ``Flow.add a tk lattice flow`` binds environment ``a`` to token ``tk`` in ``flow``.
   If ``flow`` already contains a binding for token ``tk``, it is joined with ``a`` using ``lattice.join``.

.. describe:: Flow.remove : token -> 'a flow -> 'a flow

   ``Flow.remove tk flow`` removes token ``tk`` from ``flow``.

.. describe:: Flow.copy : token -> token -> 'a lattice -> 'a flow -> 'a flow -> 'a flow

   ``Flow.copy tk1 tk2 lattice flow1 flow2`` copies the environment associated to token ``tk1`` in ``flow1`` into token ``tk2`` in ``flow2``.
   It is equivalent to ``Flow.set tk2 (Flow.get tk1 lattice flow1) lattice flow2``.

.. describe:: Flow.rename : token -> token -> 'a lattice -> 'a flow -> 'a flow

   ``Flow.rename tk1 tk2 flow`` renames the binding ``(tk1, a)`` in ``flow`` into ``(tk2, a)``.
   It is equivalent to ``Flow.add tk2 (Flow.get tk1 lattice flow) (remove tk1 flow)``.

.. describe:: Flow.mem: token -> 'a flow -> bool

   Token membership test. 

.. describe:: Flow.filter : (token -> 'a -> bool) -> 'a flow -> 'a flow

   ``Flow.filter f flow`` returns a flow with all binding ``(tk, a)`` in ``flow`` verifying ``f tk a = true``.

.. describe:: Flow.partition : (token -> 'a -> bool) -> 'a flow -> 'a flow * 'a flow

   ``Flow.partition f flow`` returns a pair of flows ``(flow1, flow2)`` where ``flow1`` (resp. ``flow2``) contains all bindings ``(tk, a)`` in ``flow`` that verify ``f tk a = true`` (reps. ``f tk a = false``).

.. describe:: Flow.map : (token -> 'a -> 'a) -> 'a flow -> 'a flow

   ``Flow.map f flow`` returns a new flow where each binding ``(tk, a)`` in ``flow`` is changed into ``(tk, f a)``.

.. describe:: Flow.fold : ('b -> token -> 'a -> 'b)  -> 'b -> 'a flow -> 'b

   ``Flow.fold f init flow`` computes ``f (... (f (f init tk1 a1) tk2 a2) ...) tkN aN``, where ``(tk1, a1) ... (tkN, aN)`` are binding of ``flow`` in increasing token order. 


Context
-------

During an analysis, abstract domains may need to maintain a global state that doesn't depend on the control flow.
For example, domains may need to access information about the currently analyzed function (its name, return type, *etc.*) or to keep a cache of previously computed values.
Instead of creating global mutable variables, Mopsa provides a functional mechanism to do that, called the *context*.
The context is a heterogeneous data store given to every transfer function.
In contrast to the abstract environment, the context is propagated in a flow-insensitive way.
For example, when analyzing the code ``if (cond) { .. } else { .. }``, the context at the end of the *then* branch is propagated at the beginning of the *else* branch.

The context is stored in values of type ``'a ctx``, defined in `framework/core/context.ml <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/context.ml>`_.
It is saved in the flow and can be accessed using the following API :

.. describe:: Flow.get_ctx : 'a flow -> 'a ctx

   Get the context from a flow.

.. describe:: Flow.set_ctx : 'a ctx -> 'a flow -> 'a flow

   Set the context of a flow.

In order to store/retrieve values from the context, domains need to register context keys by extending the type ``('a, 'v) ctx_key``, where the type variable ``'a`` represents the type of the global abstraction, and ``'v`` the type of value associated to context key.
Registration is done using the utility functor ``GenContextKey``:

.. code-block:: ocaml

   module GenContextKey
     (Value : sig
       type 'a t
       val print : (Print.printer -> 'a -> unit) -> Format.formatter -> 'a t -> unit
     end)
   :
   sig
     val key : ('a,'a Value.t) ctx_key
   end
   

For example, the framework defines a context key to store the current callstack as follows (see :ref:`here<Callstacks>` for a description of the Callstacks API):

.. code-block:: ocaml

   module CallstackKey = GenContextKey
    (struct
      type 'a t = callstack
      let print pp fmt cs = pp_callstack fmt cs
    end)

    let callstack_ctx_key = CallstackKey.key

Given a flow ``f``, one can retrieve the current callstack as follows:

.. code-block:: ocaml

   let ctx = Flow.get_ctx f in
   let cs = find_ctx callstack_ctx_key ctx in
   ...

The full Context API is described bellow:

.. describe:: empty_ctx : 'a ctx

   Empty context.

.. describe:: singleton_ctx : ('a,'v) ctx_key -> 'v -> 'a ctx

   Create a context with one element.

.. describe:: mem_ctx : ('a,'v) ctx_key -> 'a ctx -> bool

   Checks if a context contains an entry for the given key.

.. describe:: find_ctx : ('a,'v) ctx_key -> 'a ctx -> 'v

   ``find_ctx k ctx`` returns the element at key ``k`` in the context ``ctx``. Raises ``Not_found`` if no element is found.

.. describe:: find_ctx_opt : ('a,'v) ctx_key -> 'a ctx -> 'v option

   Similar to ``find_ctx`` but returns ``None`` if no element is found.

.. describe:: add_ctx : ('a,'v) ctx_key -> 'v -> 'a ctx -> 'a ctx

   ``add_ctx k v ctx`` adds element ``v`` at key ``k`` in the context ``ctx``. The previous element is overwritten if present.

.. describe:: remove_ctx : ('a,'v) ctx_key -> 'a ctx -> 'a ctx

   ``add_ctx k v ctx`` removes the element at key ``k`` in the context ``ctx``. If key ``k`` is not in ``ctx``, ``ctx`` is returned unchanged.
