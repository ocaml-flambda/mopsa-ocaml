(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Flows are a trace abstraction based on continuations to represent
   executions that reach the current control location or that have
   been suspended.  Each continuation is identified by a token that
   encodes the trace end point. A flow is then a map from tokens to
   abstract elements over-approximating the traces reaching that point. *)

type token = Manager.token
(** Flow tokens *)

type 'a flow = 'a Manager.flow
(** Flow abstraction *)

val bottom    : 'a flow
(** Empty set of flows *)

val top       : 'a flow
(** Set of all possible flows *)

val is_bottom : ('a, _) Manager.man -> 'a flow -> bool
(** Emptiness test *)

val subset    : ('a, _) Manager.man -> 'a flow -> 'a flow -> bool
(** Inclusion test *)

val join      : ('a, _) Manager.man -> 'a flow -> 'a flow -> 'a flow
(** Abstract union operator *)

val meet      : ('a, _) Manager.man -> 'a flow -> 'a flow -> 'a flow
(** Abstract intersection operator *)

val widen     : ('a, _) Manager.man -> 'a flow -> 'a flow -> 'a flow
(** Widening operator *)

val print     : ('a, _) Manager.man -> Format.formatter -> 'a flow -> unit
(** Pretty printer *)

val get       : token -> ('a, _) Manager.man -> 'a flow -> 'a
(** [get tk man flow] returns the abstract element associated to token
   [tk] in [flow]. Returns [man.bottom] the binding is not found. *)

val set       : token -> 'a -> ('a, _) Manager.man -> 'a flow -> 'a flow
(** [set tk a man flow] overwrites the binding of token [tk] in [flow]
   with the abstract element [a]. *)

val add       : token -> 'a -> ('a, _) Manager.man -> 'a flow -> 'a flow
(** [add tk a man flow] appends (by union) [a] to the existing binding
   of [tk] in [flow].  It is equivalent to [set tk (man.join a (get tk
   man flow)) man flow] *)

val remove    : token -> ('a, _) Manager.man -> 'a flow -> 'a flow
(** [remove tk man flow] removes token [tk] from the map of [flow] *)

val filter    : (token -> 'a -> bool) -> ('a, _) Manager.man -> 'a flow -> 'a flow
(** [filter f man flow] keeps in [flow] all tokens [tk] verifying [f tk = true] *)

val map       : (token -> 'a -> 'a) -> ('a, _) Manager.man -> 'a flow -> 'a flow

val fold      : ('b -> token -> 'a -> 'b)  -> 'b -> ('a, _) Manager.man -> 'a flow -> 'b

val merge     : (token -> 'a option -> 'a option -> 'a option) -> ('a, _) Manager.man -> 'a flow -> 'a flow -> 'a flow
