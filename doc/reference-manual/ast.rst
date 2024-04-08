AST
===

To support the analysis of multiple languages, the AST (*Abstract Syntax Tree*) in Mopsa is extensible.
Developers can easily add new AST nodes, such as expressions, and statements.
By doing so, Mopsa is not limited to a common intermediate representation to which all languages should be translated, which allows capturing the full semantics of the analyzed program.

AST extensions are based on OCaml's extensible types.
Each time the developer extends the AST with a new node, some utility functions need be registered, such as a pretty-printer, a structural comparison and a visitor.

Types
-----

Types of expressions in Mopsa are represented by the extensible type ``typ``, defined in `framework/core/ast/type.mli <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/ast/typ.mli>`_.
To define a new type, extend ``typ`` with a new constructor and register it using the function ``register_typ``.

For example, to add a new type ``T_int`` for integers, first extend ``typ``:

.. code-block:: ocaml

   type typ += T_int

Then, register the new type using ``regiser_typ``:

.. code-block:: ocaml

   register_typ {
     print   = (fun next fmt -> function
       | T_int -> Format.pp_print_string fmt "int"
       | t     -> next fmt t
     );
     compare = (fun next -> next);
   };;

Extensible types in Mopsa are registered in a chain.
The parameter ``next`` of ``print`` represents the printer function of the next extensible type in the chain.
Thus, if the given type doesn't correspond to the type currently registered, we need to go further in the chain to find the actual match.
The same process applies to ``compare``.

.. note::

   Note that in this case ``compare`` calls directly ``next`` without matching the type ``T_int``.
   This is correct because ``T_int`` doesn't have parameters, and the fallback behavior of ``compare`` is to call ``Stdlib.compare``, which doesn't perform a deep structural comparison.

To print or compare Mopsa types, the framework provides generic functions that work on any registered type:

.. code-block:: ocaml

   val pp_typ      : Format.formatter -> typ -> unit
   val compare_typ : typ -> typ -> int


Constants
---------

Constants are defined by extending the type ``constant`` (found in `framework/core/ast/constant.mli <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/ast/constant.mli>`_) and performing registration using ``register_constant``.

The following example shows how to register integer constants:

.. code-block:: ocaml

   type constant += C_int of int

   register_constant {
     print   = (fun next fmt -> function
       | C_int n -> Format.pp_print_int fmt n
       | c       -> next fmt c
     );
     compare = (fun next c1 c2 ->
       match c1, c2 with
       | C_int n1, C_int n2 -> Int.compare n1 n2
       | _                  -> next c1 c2
     );
   };;

Similarly to types, registration is performed by chaining ``print`` and ``compare`` functions with other definitions.
The framework provides the following generic functions for any registered constant:

.. code-block:: ocaml

   val pp_constant      : Format.formatter -> constant -> unit
   val compare_constant : constant -> constant -> int


Expressions
-----------

Expressions in Mopsa have type ``expr`` defined in `framework/core/ast/expr.mli <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/ast/expr.mli>`_ as:

.. code-block:: ocaml

   type expr = {
     ekind    : expr_kind;           (* Kind of the expression *)
     etyp     : typ;                 (* Type of the expression *)
     erange   : range;               (* Location range of the expression *)
     etrans   : expr SemanticMap.t;  (* Translations to other semantics *)
     ehistory : expr list;           (* Rewriting history *)
   }

The field ``ekind`` gives the kind of the expression, which is an extensible type used to define new expressions.
The field ``etyp`` gives the type of the expression and ``erange`` its location range within the source file (more details on locations can be found :ref:`here<Locations>`).
Fields ``etrans`` and ``ehistory`` are related to expression evaluation, which will be presented :ref:`later<Expression Evaluation>`.

To define a new kind of expressions, extend ``expr_kind`` and call ``register_expr`` to register it, similarly to other extensible AST nodes described previously.
For example, to register a new expression representing an array access ``a[i]``, proceed as follows:

.. code-block:: ocaml

   type expr_kind += E_array_access of expr (* array *) * expr (* index *)

   register_expr {
      print    = (fun next fmt -> function
        | E_array_access(a,i) ->
          Format.fprintf fmt "%a[%a]" pp_expr a pp_expr i
        | e -> next fmt e
      );
      compare  = (fun next e1 e2 ->
        match e1, e2 with
        | E_array_access(a1,i1), E_array_access(a2,i2) ->
          Mopsa_utils.Compare.pair compare_expr compare_expr (a1,i1) (a2,i2)
        | _ -> next e1 e2
      );
   };;

As illustrated in this example, you can use two generic functions for printing and comparing any Mopsa expression:

.. code-block:: ocaml

   val pp_expr      : Format.formatter -> expr -> unit
   val compare_expr : expr -> expr -> int

The utility function ``Mopsa_utils.Compare.pair`` allows comparing two pairs using comparison functions on elements, as described :ref:`here<Comparison>`.

.. note::

   The function ``compare_expr`` considers the kind ``ekind`` only when comparing two expressions.
   The other fields are ignored.

Variables
---------

Variables are defined in `framework/core/ast/var.mli <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/ast/var.mli>`_ as:

.. code-block:: ocaml

   type var = {
     vname     : string;   (** Unique name of the variable*)
     vtyp      : typ;      (** Type of the variable *)
     vkind     : var_kind; (** Kind the variable *)
     vmode     : mode;     (** Access mode of the variable *)
     vsemantic : semantic; (** Semantic of the variable *)
   }

The field ``vname`` is the name of the variable.
This name should be unique in the environment.
The field ``vtyp`` gives the type of the variable.
The field ``vsemantic`` is related to expression evaluation, which will be described :ref:`later<Expression Evaluation>`.
Let's explain in more details what fields ``vkind`` and ``vmode`` represent.

Kind of Variables
^^^^^^^^^^^^^^^^^

The field ``vkind`` represents the kind of the variable, which is an extensible type used to annotate variables with additional information.
Such information can be provided by the parser for example.
Consider the case of the analysis of C.
Variables are annotated with the kind ``V_cvar`` defined in `lang/c/ast.ml <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/lang/c/ast.ml>`_, that gives information provided by the parser about the declaration range and the scope of the variable:

.. code-block:: ocaml

   type cvar = {
     cvar_scope: c_var_scope; (* scope of the variable *)
     cvar_range: range;       (* declaration range *)
     cvar_uid: int;           (* unique identifier *)
     cvar_orig_name : string; (* original name *)
     cvar_uniq_name : string; (* unique name *)
   }

   and c_var_scope =
    | Variable_global                  (* global among translation units *)
    | Variable_extern                  (* declared but not defined *)
    | Variable_local of c_fundec       (* local to a function *)
    | Variable_parameter of c_fundec   (* formal argument *)
    | Variable_file_static of string   (* restricted to a translation unit *)
    | Variable_func_static of c_fundec (* restricted to a function *)

   type var_kind += V_cvar of cvar

Registration of new variable kinds is done with the function ``register_var``:

.. code-block:: ocaml

   register_var {
     print = (fun next fmt v ->
       match v.vkind with
       | V_cvar cvar ->
         Format.pp_print_string fmt cvar.cvar_orig_name
       | _ -> next fmt v
     );
     compare = (fun next v1 v2 ->
       match v1.vkind, v2.vkind with
       | V_cvar cvar1, V_cvar cvar2 ->
         Int.compare cvar1.cvar_uid cvar2.cvar_uid
       | _ -> next v1 v2
     );
   };;

Note that the printing function uses the original name of the variable ``cvar_orig_name``, while comparing two variables relies on their unique identifier ``cvar_uid``.

Another usage of variable kinds is the creation of auxiliary variables that encode a semantic property in an abstract domain.
For example, in a smashing abstraction of C arrays, we need to introduce an auxiliary variable representing a summary of the values in the array:

.. code-block:: ocaml

   type var_kind += V_c_array_smash of var

   register_var {
     print = (fun next fmt v ->
       match v.vkind with
       | V_c_array_smash(a) ->
         Format.fprintf fmt "smash(%a)" pp_var a
       | v -> next fmt v
     );
     compare = (fun next v1 v2 ->
       match v1.vkind, v2.vkind with
       | V_c_array_smash(a1), V_c_array_smash(a2) ->
         compare_var a1 a2
       | _ -> next v1 v2
     );
   }


Access Mode
^^^^^^^^^^^

Auxiliary variables may represent a summary of several concrete variables.
In this case, updating the value of a concrete variable should not overwrite the value of the summary, otherwise the abstraction becomes unsound.
Instead, a *weak update* should be performed that accumulates the new assigned value with the previous ones.
This behavior is determined by the field ``vmode:mode``:

.. code-block:: ocaml

   type mode = STRONG | WEAK

When a variable has its field ``vmode`` set to ``STRONG``, assignments to the variable will overwrite its value.
On the other hand, a variable with a mode ``WEAK`` will be modified only via weak updates, preserving its accumulated values.

However, in some situations, we need to perform strong updates even on summary variables.
For example, when declaring a C array ``int a[] = {1, 2, 3};``, we need to initialize the smash of ``a`` with value ``1`` using a strong update, before accumulating the remaining values using weak updates.
To do so, Mopsa defines the expression representing variables as follows:

.. code-block:: ocaml

   type expr_kind += E_var of var * mode option

By default, the expression for a variable ``v`` is ``E_var(v, None)``, which will use the access mode of ``v`` when assigning a value to it.
It is possible to overload this access mode by specifying it explicitly in the expression ``E_var(v, Some mode)``.
For example, to force a strong update on a ``V_array_smash`` variable, one can use the expression ``E_var(v, Some STRONG)``.

Statements
----------

Statements are defined in `framework/core/ast/stmt.mli <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/ast/stmt.mli>`_ as:

.. code-block:: ocaml

   type stmt = {
     skind : stmt_kind; (** kind of the statement *)
     srange : range;    (** location range of the statement *)
   }

   and stmt_kind = ..

The field ``srange`` gives the location range of the statement within the source code.
The field ``skind`` is the kind of the statement, which is an extensible type that allows adding new statements to the analyzer.
Similarly to other constructs, adding a new kind of statements is done by extending the type ``stmt_kind`` and registering a ``print`` and a ``compare`` function.
For example, adding the assignment statement is done as follows:

.. code-block:: ocaml

   type stmt_kind += S_assign of expr (* left-hand side *)* expr (* right-hand side *)

   register_stmt {
     print = (fun next fmt s ->
       match s.skind with
       | S_assign(x,e) ->
         Format.fprintf fmt "%a = %a;" pp_expr x pp_expr e
       | _ -> next fmt s
     );
     compare = (fun next s1 s2 ->
       match s1.skind, s2.skind with
       | S_assign(x1,e1), S_assign(x2,e2) ->
         Compare.pair compare_expr compare_expr (x1,e1) (x2,e2)
       | _ -> next s1 s2
     );
   };;

Visitors
--------

Mopsa provides generic functions for visiting expressions and statements, defined in `framework/core/ast/visitor.mli <https://gitlab.com/mopsa/mopsa-analyzer/-/blob/master/analyzer/src/mopsa_analyzer/framework/core/ast/visitor.mli>`_.
Two kinds of visitors are defined: *predicates* and *iterators*.

Predicate Visitors
^^^^^^^^^^^^^^^^^^

Predicate visitors check whether sub-expressions and sub-statements verify some boolean conditions:

.. code-block:: ocaml

   val exists_expr  : (expr -> bool) -> (stmt -> bool) -> expr -> bool
   val exists_stmt  : (expr -> bool) -> (stmt -> bool) -> stmt -> bool
   val for_all_expr : (expr -> bool) -> (stmt -> bool) -> expr -> bool
   val for_all_stmt : (expr -> bool) -> (stmt -> bool) -> stmt -> bool

For example, the following code checks whether an expression ``e`` contains a variable and no sub-statement:

.. code-block:: ocaml

   exists_expr
     (fun e' ->
       match e'.ekind with
       | E_var _ -> true
       | _       -> false)
     (fun s' -> false)
     e

Iterator Visitors
^^^^^^^^^^^^^^^^^

Mopsa provides the following functions to iterate over the parts of expressions and statements:

.. code-block:: ocaml

   val map_expr :
     (expr -> expr visit_action) ->
     (stmt -> stmt visit_action) ->
     expr -> expr

   val map_stmt :
     (expr -> expr visit_action) ->
     (stmt -> stmt visit_action) ->
     stmt -> stmt

   val fold_expr :
     ('a -> expr -> 'a visit_action) ->
     ('a -> stmt -> 'a visit_action) ->
     'a -> expr -> 'a

   val fold_stmt :
     ('a -> expr -> 'a visit_action) ->
     ('a -> stmt -> 'a visit_action) ->
     'a -> stmt -> 'a

   val fold_map_expr :
     ('a -> expr -> ('a * expr) visit_action) ->
     ('a -> stmt -> ('a * stmt) visit_action) ->
     'a -> expr -> 'a * expr

   val fold_map_stmt :
     ('a -> expr -> ('a * expr) visit_action) ->
     ('a -> stmt -> ('a * stmt) visit_action) ->
     'a -> stmt -> ('a * stmt)

The functions ``map_expr`` and ``map_stmt`` transform an expression or a statement into another one by applying a transformer on each sub-expression and sub-statement.
The functions ``fold_expr`` and ``fold_stmt`` iterate over the parts of an expression or a statement while propagating an arbitrary accumulator value.
The functions ``fold_map_expr`` and ``fold_map_stmt`` combine ``map`` and ``fold`` by transforming an expression or a statement while propagating an accumulator value.

To use these functions, you need to define *visit actions* on sub-parts (expressions and statements), which are functions returning one of these values:

.. code-block:: ocaml

   type 'a visit_action =
   | Keep       of 'a (* Keep the given result *)
   | VisitParts of 'a (* Continue visiting the parts of the given result *)
   | Visit      of 'a (* Iterate the visitor on the given result *)
