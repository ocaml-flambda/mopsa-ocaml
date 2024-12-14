(* Lattice definition *)

type t
(** Type of an abstract elements. *)

val bottom: t
(** Least abstract element of the lattice. *)

val top: t
(** Greatest abstract element of the lattice. *)

val print: Format.formatter -> t -> unit
(** Printer of an abstract element. *)

val is_bottom: t -> bool
(** [is_bottom a] tests whether [a] is bottom or not. *)

val subset: t -> t -> bool
(** Partial order relation. [subset a1 a2] tests whether [a1] is
    related to (or included in) [a2]. *)

val join: t -> t -> t
(** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

val meet: t -> t -> t
(** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

val widen: 'a ctx -> t -> t -> t
(** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
    ensures stabilization of ascending chains. *)

val merge : t -> t * block -> t * block -> t
(** [merge pre (post1, log1) (post2, log2)] synchronizes two divergent
    post-conditions [post1] and [post2] using a common pre-condition [pre].

    Diverging post-conditions emerge after a fork-join trajectory in the
    abstraction DAG (e.g., a reduced product).

    The logs [log1] and [log2] represent a journal of internal statements
    executed during the the computation of the post-conditions over the
    two trajectories.
*)

(* Domain identification *)

val id : t id
(** Domain identifier *)

val name : string
(** Domain name *)

(* Abstract transformers *)

val init : program -> t
(** Initial abstract element *)

val exec : stmt -> ('a,t) simplified_man -> 'a ctx -> t -> t option
(** Computation of post-conditions *)

val ask : ('a,'r) query -> ('a,t) simplified_man -> 'a ctx -> t -> 'r option
(** Handler of queries *)
