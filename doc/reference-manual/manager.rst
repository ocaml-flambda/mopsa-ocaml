Manager
=======

The manager encapsulates the lattice definition and the transfer functions of the global abstraction during the analysis.
It represents the solely means for domains to access the analysis framework.
Each domain is provided with its own instance of the manager that allows it to communicate with other domains with a very low coupling.
The type of the manager is defined in `framework/core/manager.ml <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/manager.ml>`_ as follows:

.. code-block:: ocaml

    type ('a, 't) man = {
	lattice : 'a lattice;
	get : 'a -> 't;
	set : 't -> 'a -> 'a;
	exec : ?route:route -> stmt -> 'a flow -> 'a post;
	eval : ?route:route -> ?translate:semantic -> expr -> 'a flow -> 'a eval;
	ask : 'r. ?route:route -> ('a,'r) query -> 'a flow -> 'r;
	print_expr : ?route:route -> 'a flow -> (printer -> expr -> unit);
    }

The type variable ``'a`` represents the type of the global abstraction, while ``'t`` represents the type of the abstract element of the domain.
Note that an instance of the manager for a given domain will have the type variable ``'a`` polymorphic (*i.e.* the domain is not tied to a specific global abstraction), while ``'t`` will be instantiated with the concrete type of the abstraction of the domain.  
The field ``lattice`` gives access to the lattice operators defined for the global abstraction ``'a``, as described :ref:`previously<Lattice>`.
In the following, we describe the remaining fields.

Accessors
---------

The fields ``get`` and ``set`` are the accessors that allow the domain to access its own abstract element within the global abstraction.
The following examples illustrates how a domain can retrieve its abstract element from the current environments in a flow:

.. code-block:: ocaml

   let cur = Flow.get T_cur flow in
   let abs = man.get cur in
   ...

In addition to accessors, the framwork provides the following utility functions to simplify some recurring patterns:

.. describe:: set_env : token -> 't -> ('a,'t) man -> 'a flow

   Set the abstract element of a domain in a flow. ``set_env tk abs man flow`` is equivalent to ``Flow.set tk (man.set abs (man.get (Flow.get tk man.lattice flow))) man.lattice flow``.

.. describe:: get_env : token -> ('a,'t) man -> 'a flow -> 't

   Get the abstract element of a domain in a flow. ``get_env tk man flow`` is equivalent to ``man.get (Flow.get tk man.lattice flow)``.

.. describe:: map_env : token -> ('t -> 't) -> ('a,'t) man -> 'a flow -> 'a flow

   Apply a transformation on the abstract element of a domain in a flow. ``map_env tk f man flow`` is equivalent to ``set_env tk (f (get_env tk man flow)) man flow``.


Routes
------

Before describing how to apply transfer functions (e.g. executing a statement), it is important to understand the concept of *routes*.
By default, requests for transfer functions are broadcasted to all domains.
Routes allows a domain to select which domains should execute a transfer function.
However, to keep the coupling as low as possible, target domains are not designated by name explicitly.
Instead, they are identified either by their concrete semantics or by their relative position w.r.t. the calling domain, as shown by the type ``route`` defined in `framework/core/route.ml <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/route.ml>`_:

.. code-block:: ocaml

   type route =
     | Below    of domain
     | Semantic of semantic

   and domain   = string
   and semantic = string

The route ``Below dom`` forwards the transfer function call to the sub-tree below domain ``dom`` in the current :ref:`configuration<Configuration>`.
The route ``Semantic sem`` forwards the transfer function call to the sub-tree identified by the semantic ``sem`` in the current :ref:`configuration<Configuration>`.
For example, consider the default configuration of C found in `share/mopsa/configs/c/default.json <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/share/mopsa/configs/c/default.json>`_.
It contains a sub-tree identified by the semantic ``Universal``:

.. code-block:: json

    {
	"semantic": "Universal",
	"switch": [
	    "universal.iterators.intraproc",
	    "universal.iterators.loops",
	    "universal.iterators.interproc.inlining",
	    "universal.iterators.unittest",
	    {
		"nonrel": {
		    "union": [
			"universal.numeric.values.intervals.float",
			"universal.numeric.values.intervals.integer"
		    ]
		}
	    }
	]
    }

C domains can call ``man.exec stmt flow ~route:(Semantic "Universal")`` in order to forward the statement to this particular sub-tree.
Assume that the statement is an assignment and is caught by the domain ``universal.iterators.intraproc``.
The domain performs some simplifications, before forwarding the simplified statement to its sub-tree by calling ``man.exec stmt' flow ~route:(Below "universal.iterators.intraproc")``.
Finally, the simplified statement will be handled by the non-relational domain.

.. important::

   Routes are useful for two main reasons:

   - They can improve analyzer performance since they avoid going through all domains when searching for the appropriate target of the transfer function. In this case, their use is not not mandatory.
   - They can be necessary when a domain acts as a decorator above other domains. In such situations, when the decorator domain handles the execution of a statement for example, it may want to update its local state before forwarding the same statement to the other domains. In this case, it is necessary to use the route ``Below``, otherwise the decorator domain will receive again the statement, leading to a non-terminating loop.


Statement Execution
-------------------

The field ``exec`` computes the post-state of execution of a statement in a given flow abstraction.
When a domain calls ``exec``, the framework broadcasts this request to all domains in a particular order, as specified by the :ref:`configuration<Configuration>`.
The returned value of type ``'a post`` represents a DNF of ``'a flow``, *i.e.* the replying domain can perform a case analysis and create separate partitions to improve precision.
As explained :ref:`later<Cases>`, the cases in ``'a post`` are encapsulated in a monad.
Domains can use the binding operator ``>>%`` to continue execution on separate cases.

For instance, consider the following code:

.. code-block:: ocaml

   man.exec stmt flow >>% fun flow' ->
   let abs = get_env T_cur man flow' in
   let abs' = apply_some_transformations abs in
   let flow'' = set_env T_cur abs' man flow' in
   Post.return flow''

The statement ``stmt`` is executed in flow ``flow``.
After getting the result, the calling domain binds each case with a function.  
Thus, the parameter ``flow'`` represents one case of the post-state.
The calling domain retrieves its private abstract element from ``'flow``, applies some transformations, puts it back in ``flow'`` before returning the result.  
So the calling domain is independent on how many cases the replying domain generated, as it needs only to provide how one case is handled.
After that, the framework will collect the result of each case and merge them into one flow abstraction.

.. note::

   Note that there is no direct dependency between domains: they communicate only through a concrete semantics of a language (here through a statement).
   The calling domain doesn't depend on the abstraction applied by the replying domain.
   This is important to build a modular analysis where domains can be changed easily.

Here is the main functions of the Post API:

.. describe:: Post.return : 'a flow -> 'a post

   Return a post-state with a singleton flow.

.. describe:: Post.join : 'a post -> 'a post -> 'a post

   Join two post-states.

.. describe:: Post.join_list : empty:(unit -> 'a post) -> 'a post list -> 'a post

   Join a list of post-states. The parameter ``empty`` is called when the list is empty.

.. describe:: Post.meet : 'a post -> 'a post -> 'a post

   Intersect two post-states.

.. describe:: Post.meet_list : empty:(unit -> 'a post) -> 'a post list -> 'a post

   Intersect a list of post-states. The parameter ``empty`` is called when the list is empty.

.. describe:: Post.bind : ('a flow -> ('a,'r) cases) -> 'a post -> ('a,'r) cases

   Bind a post-state by a applying a function on each case.

.. describe:: (>>%) : 'a post -> ('a flow -> ('a,'r) cases) -> ('a,'r) cases

   Similar to ``Post.bind``.

.. describe:: Post.bind_opt : ('a flow -> ('a,'r) cases option) -> 'a post -> ('a,'r) cases option

   Similar to ``Post.bind`` but accepts a function that returns an option result. Useful for defining partial functions, as explained :ref:`here<Standard>`.

.. describe:: val (>>%?) : 'a post -> ('a flow -> ('a,'r) cases option) -> ('a,'r) cases option

   Similar to ``Post.bind_opt``.


Expression Evaluation
---------------------

Cleaners
^^^^^^^^

Queries
-------

Invariant Printer
-----------------
