(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Flows are handled continuations and are identified by a finite
   set of tokens. We use a partitioning abstraction to aggregate together
   the abstract environments of each flow token.
*)


(*==========================================================================*)
                           (** {2 Tokens} *)
(*==========================================================================*)


type token = ..

type token += TCur

val register_token_compare : ((token -> token -> int) -> token -> token -> int) -> unit

val compare_token : token -> token -> int

val register_pp_token : ((Format.formatter -> token -> unit) -> Format.formatter -> token -> unit) -> unit

val pp_token : Format.formatter -> token -> unit

(*==========================================================================*)
                           (** {2 Flows map} *)
(*==========================================================================*)


type 'a flow

val bottom : 'a flow

val top : 'a flow

val is_bottom : 'a flow ->is_value_bottom:('a -> bool) -> bool

val is_top : 'a flow -> bool

val leq : 'a flow -> 'a flow -> is_value_bottom:('a -> bool) -> value_leq:('a -> 'a -> bool) -> bool

val join : 'a flow -> 'a flow -> value_join:('a -> 'a -> 'a) -> 'a flow

val meet : 'a flow -> 'a flow -> value_bottom:'a -> value_meet:('a -> 'a -> 'a) -> 'a flow

val widening : Context.context -> 'a flow -> 'a flow -> value_widening:(Context.context -> 'a -> 'a -> 'a) -> 'a flow

val print : value_print:(Format.formatter -> 'a -> unit) -> Format.formatter -> 'a flow -> unit

val get : token -> 'a flow -> value_bottom:'a -> value_top:'a -> 'a

val set : token -> 'a -> 'a flow -> is_value_bottom:('a -> bool) -> 'a flow

val remove : token -> 'a flow -> 'a flow

val filter : (token -> 'a -> bool) -> 'a flow -> 'a flow

val add : token -> 'a -> 'a flow -> is_value_bottom:('a -> bool) -> value_join:('a -> 'a -> 'a) -> 'a flow

val map : (token -> 'a -> 'b) -> 'a flow -> 'b flow

val fold : (token -> 'a -> 'b -> 'b) -> 'a flow -> 'b -> 'b

val merge : (token -> 'a option -> 'a option -> 'a option) -> 'a flow -> 'a flow -> value_bottom:'a -> 'a flow
