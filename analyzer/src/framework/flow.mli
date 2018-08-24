(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Flows is a trace partitioning the collect environments depending
    on the kind of the control flow. Not only reaching environments are
    concerned, but also environments of traces suspended at previous
    control points are kept in the flow map.  Each kind of control flow
    is identified with a unique token.

    Flow insensitive annotations are also maintained in the flow
    abstraction.  
*)

open Annotation
open Manager

type token = Manager.token
(** Flow tokens *)

type 'a flow = 'a Manager.flow
(** Flow abstraction *)

val bottom : 'a annot -> 'a flow
(** Empty set of flows *)

val top : 'a annot -> 'a flow
(** Set of all possible flows *)

val is_bottom : ('a, _) man -> 'a flow -> bool
(** Emptiness test *)

val subset : ('a, _) man -> 'a flow -> 'a flow -> bool
(** Inclusion test *)

val join : ('a, _) man -> 'a flow -> 'a flow -> 'a flow
(** Abstract union operator *)

val meet : ('a, _) man -> 'a flow -> 'a flow -> 'a flow
(** Abstract intersection operator *)

val widen : ('a, _) man -> 'a flow -> 'a flow -> 'a flow
(** Widening operator *)

val print : ('a, _) man -> Format.formatter -> 'a flow -> unit
(** Pretty printer *)

val get : token -> ('a, _) man -> 'a flow -> 'a
(** [get tk man flow] returns the abstract element associated to token
   [tk] in [flow]. Returns [man.bottom] the binding is not found. *)

val set : token -> 'a -> ('a, _) man -> 'a flow -> 'a flow
(** [set tk a man flow] overwrites the binding of token [tk] in [flow]
   with the abstract element [a]. *)

val add : token -> 'a -> ('a, _) man -> 'a flow -> 'a flow
(** [add tk a man flow] appends (by union) [a] to the existing binding
   of [tk] in [flow].  It is equivalent to [set tk (man.join a (get tk
   man flow)) man flow] *)

val remove    : token -> ('a, _) man -> 'a flow -> 'a flow
(** [remove tk man flow] removes token [tk] from the map of [flow] *)

val filter    : (token -> 'a -> bool) -> ('a, _) man -> 'a flow -> 'a flow
(** [filter f man flow] keeps in [flow] all tokens [tk] verifying [f tk = true] *)

val map : (token -> 'a -> 'a) -> ('a, _) man -> 'a flow -> 'a flow

val fold : ('b -> token -> 'a -> 'b)  -> 'b -> ('a, _) man -> 'a flow -> 'b

val merge : (token -> 'a option -> 'a option -> 'a option) -> ('a, _) man -> 'a flow -> 'a flow -> 'a flow

val set_domain_env : token -> 't -> ('a, 't) man -> 'a flow -> 'a flow
(** [set_domain_env tk a man flow] overwrites the local part of a domain in
   the abstract element bound to token [tk] in [flow] *)

val get_domain_env : token -> ('a, 't) man -> 'a flow -> 't
(** [get_domain_env tk man flow] retrieves the local part of a domain in the
   abstract element bound to token [tk] in [flow] *)

val map_domain_env : token -> ('t -> 't) -> ('a, 't) man -> 'a flow -> 'a flow
(** [map_domain_env tk f man flow] is equivalent to [set_domain_env tk (f (get_env tk man flow)) man flow] *)
