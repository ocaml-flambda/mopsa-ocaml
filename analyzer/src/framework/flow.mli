(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Flows are a trace abstraction based on continuations partitioned
   by a finite set of tokens. *)

type token = Manager.token
type 'a flow = 'a Manager.flow

val bottom    : 'a flow

val top       : 'a flow

val is_bottom : ('a, _) Manager.man -> 'a flow -> bool

val is_top    : ('a, _) Manager.man -> 'a flow -> bool

val leq       : ('a, _) Manager.man -> 'a flow -> 'a flow -> bool

val join      : ('a, _) Manager.man -> 'a flow -> 'a flow -> 'a flow

val meet      : ('a, _) Manager.man -> 'a flow -> 'a flow -> 'a flow

val widen     : ('a, _) Manager.man -> 'a flow -> 'a flow -> 'a flow

val print     : ('a, _) Manager.man -> Format.formatter -> 'a flow -> unit

val get       : ('a, _) Manager.man -> token -> 'a flow -> 'a

val set       : ('a, _) Manager.man -> token -> 'a -> 'a flow -> 'a flow

val remove    : ('a, _) Manager.man -> token -> 'a flow -> 'a flow

val filter    : ('a, _) Manager.man -> (token -> 'a -> bool) -> 'a flow -> 'a flow

val add       : ('a, _) Manager.man -> token -> 'a -> 'a flow -> 'a flow

val map       : ('a, _) Manager.man -> (token -> 'a -> 'a) -> 'a flow -> 'a flow

val fold      : ('a, _) Manager.man -> (token -> 'a -> 'b -> 'b) -> 'a flow -> 'b -> 'b

val merge     : ('a, _) Manager.man -> (token -> 'a option -> 'a option -> 'a option) -> 'a flow -> 'a flow -> 'a flow

val is_cur_bottom : ('a, _) Manager.man -> 'a flow -> bool
