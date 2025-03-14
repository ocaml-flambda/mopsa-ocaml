=======
Queries
=======

.. MOPSA developer manuel

In order to ensure the modularity of the analyzer, domains in **Mopsa**
are unaware of one another (except when combined in a reduced product,
we will come back to that). We have seen that they can however
collaborate in computing post-conditions by asking one another to
execute statements (and by evaluating expressions). However both these
mechanisms are designed to produce post-conditions, meaning that they
both yield an entire abstract state and may become quite costly. It may
be the case however that we sometimes just need to gather information
that may be known to some other domain present in the abstraction. This
information can be for example, the range of an integer variable. This
information should not be necessary for a domain to function properly
(understand soundly) as the user may have assembled together an analysis
that does not contain any domain providing that information. Access to
this information may however increase the precision and even the
performances.

Defining new queries
====================

The information exchanged between domains need to be expressed in a
language shared by all domains. It should therefore not be defined and
handled by a singular domain but by several of them. Moreover the type
of this information should be easily extensible as users may require
some new type of information for new analysis. For this reason **Mopsa**
provides an extensible ``('a, 'r) query`` type. As an example in file
``analyzer/src/lang/universal/numeric/common.ml``, this type is extended
with the new constructor ``Q_int_interval``:

.. code:: ocaml

   (* Query to evaluate the integer interval of an expression *)
   type ('a, _) query +=
     | Q_int_interval : expr -> ('a, int_itv) query 

where ``int_itv`` is the type of integer intervals (pairs of integers).
It is possible that several of the domains composing the analyzer answer
to a query. Therefore the definition of a new query also requires the
definition of operators able to unite and intersect queries.

As the ``query`` type is extensible, this definition needs to be made
dynamically at the start of the analysis. This can be made via the
``register_query`` function from module ``Mopsa``. This function takes
as single argument a ``query_info`` element which is a structure
containing a ``join`` and a ``meet`` function:

.. code:: ocaml

   type query_info = {
     join : 'a 'r. query_operator -> ('a, 'r) query -> ('a -> 'a -> 'a) -> 'r -> 'r -> 'r;
     meet : 'a 'r. query_operator -> ('a, 'r) query -> ('a -> 'a -> 'a) -> 'r -> 'r -> 'r;
   }

The arguments of these function are the following:

-  The first argument of these function is a ``query_operator``, which
   is an element of type
   ``'a 'r.('a, 'r) query -> ('a-> 'a-> 'a) -> 'r -> 'r -> 'r``. When
   defining a join (meet) operator for ``Q_int_interval`` queries, this
   query operator should be called when asked to join (meet) queries
   different from ``Q_int_interval``. Think of this function as the join
   (meet) operator for the rest of the queries;
-  The second argument is the query;
-  The third argument is a join (meet) operator for the complete
   abstract state;
-  The last two arguments are the query results that actually need to be
   joined (meet).

As an example, the definition of the join and meet operator for the
range query is shown below:

.. code:: ocaml

   let () =
     register_query {
       join = (
         let f : type a r. query_operator -> (a, r) query -> (a -> a -> a) -> r -> r -> r =
           fun next query join a b ->
             match query with
             | Q_int_interval _ -> I.join_bot a b
             | _ -> next.apply query join a b
         in
         f
       );
       meet = (
         let f : type a r. query_operator -> (a, r) query -> (a -> a -> a) -> r -> r -> r =
           fun next query meet a b ->
             match query with
             | Q_int_interval e -> I.meet_bot a b
             | _ -> next.apply query meet a b
         in
         f
       );
     }

module ``I`` contains the definition of the usual abstract operators
over integer intervals.

.. note::

   Note that the ``query`` type is parameterized by the type of the
   abstract state, allowing, for example, the definition of queries
   which returns a partitioning of the input state. See the
   ``Q_c_points_to`` query from file ``analyzer/src/lang/c/common`` for
   example.

.. note::

   If you do not wish to unite and meet query results from domains in
   the analyzer but would rather get all answers from every domains, the
   ``Dnf`` utility library from **Mopsa** provides disjunctive normal
   form over any type. Therefore you could define the result of queries
   to be of type ``t Dnf.t``, the join and meet can then be easily
   defined with ``Dnf.join`` and ``Dnf.meet``.

Querying other domains
======================

The manager, providing access to the domains, in an analyzer, to the
entire analyzer, can be used to ask queries to other domains. Indeed the
manager structure provides an ``ask`` function with signature :

.. code:: ocaml

   val ask : 'r. ('a, 'r) query -> 'a flow -> 'r

Therefore a domain wanting to discover the range of an expression ``e``
in the environment ``env`` can simply ask the other domains in the
following manner and be returned an element of type ``int_itv``.

.. code:: ocaml

   man.ask (Universal.Numeric.Common.Q_int_interval e) env

Answering to queries
====================

Every domain must define an ``ask`` function with signature:

.. code:: ocaml

   val ask : ('a, 'r) query -> ('a, t) simplified_man -> 'a ctx -> t -> 'r option

The role of this function is to answer questions from other domains. Of
course not every domain can provide an answer to every query, therefore
it is possible to answer ``None`` when asked a query.

It so happens that the equality domain we are currently defining may
answer to some range questions. Indeed, if we know that :math:`x=y`, we
may answer to domains asking for the range of expression :math:`x-y`
that it is :math:`[0; 0]` :

.. code:: ocaml

   let ask : type r. ('a,r) query -> ('a,t) simplified_man -> 'a ctx -> t -> r option =
     fun query man ctx env ->
     match query with

     | Numeric.Common.Q_int_interval({ekind = E_binop(Ast.O_minus, {ekind = E_var(v, _)}, {ekind = E_var(v', _)})}) ->
       if is_eq v v' env then
         Some (Bot.Nb (ItvUtils.IntItv.of_int 0 0))
       else 
         None 
     | _ -> None
