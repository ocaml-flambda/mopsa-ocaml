(* Lattice definition *)

type t
(* Type of the abstract value. *)

val bottom: t
(* Least abstract element of the lattice. *)

val top: t
(* Greatest abstract element of the lattice. *)

val print: Format.formatter -> t -> unit
(* Printer of an abstract element. *)

val is_bottom: t -> bool
(* [is_bottom a] tests whether [a] is bottom or not. *)

val subset: t -> t -> bool
(* Partial order relation. [subset a1 a2] tests whether [a1] is
    related to (or included in) [a2]. *)

val join: t -> t -> t
(* [join a1 a2] computes an upper bound of [a1] and [a2]. *)

val meet: t -> t -> t
(* [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

val widen: t -> t -> t
(* [widen a1 a2] computes an upper bound of [a1] and [a2] that
    ensures stabilization of ascending chains. *)

(* Domain identification *)

val id : t id
(* Identifier of the value domain *)

val name : string
(* Name of the value domain *)

val display : string
(* Display name used in debug messages *)


(* Abstract transformers *)

val constant : typ -> constant -> t option
(* Forward evaluation of constants *)

val cast : ('a,t) value_man -> typ -> expr -> t option
(* Cast an expression into a value *)

val unop : operator -> typ -> t -> t
(* Forward evaluation of unary expressions *)

val binop : operator -> typ -> t -> t -> t
(* Forward evaluation of binary expressions *)

val filter : bool -> typ -> t -> t
(* Keep values that may represent the argument truth value *)


(* {2 Backward semantics} *)
(* *********** *)

val bwd_unop : operator -> typ -> t -> t -> t
(* Backward evaluation of unary operators.
    [bwd_unop op x r] returns x':
    - x' abstracts the set of v in x such as op v is in r
      i.e., we fiter the abstract values x knowing the result r of applying
      the operation on x
*)

val bwd_binop : operator -> typ -> t -> t -> t -> (t * t)
(* Backward evaluation of binary operators.
    [bwd_binop op x y r] returns (x',y') where
    - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
    - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
      i.e., we filter the abstract values x and y knowing that, after
      applying the operation op, the result is in r
*)

val bwd_cast : ('a,t) value_man -> typ -> expr -> t -> t
(* Backward evaluation of casts.
    [bwd_cast man t e x] returns x':
    - x' abstracts the set of v in x such cast(t,v) is in the evaluation 𝔼[cast(t,e)]
      i.e., we fitter the abstract values x of expression e knowing the evaluation of cast(t,e)
*)

val predicate : operator -> bool -> typ -> t -> t
(* Backward evaluation of unary boolean predicates.
    [predicate op x true] returns the subset of x such that x is
    true.
    [predicate op x false] is similar, but assumes that the predicate is false
*)

val compare : operator -> bool -> typ -> t -> t -> t * t
(* Backward evaluation of boolean comparisons. [compare op x y true] returns (x',y') where:
    - x' abstracts the set of v  in x such that v op v' is true for some v' in y
    - y' abstracts the set of v' in y such that v op v' is true for some v  in x
      i.e., we filter the abstract values x and y knowing that the test is true

     [compare op x y false] is similar, but assumes that the test is false
*)

val ask : ('a,t) value_man -> ('a,'r) query -> 'r option
(* Query handler *)

