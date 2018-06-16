(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


type 'a t = {
  flow      : 'a Flow.flow;            (** Post-condition flow *)
  mergers   : Ast.stmt list;           (** Meet mergers *)
}
(** Post-condition of [exec] transfer functions *)

val of_flow : ?mergers:Ast.stmt list -> 'a Flow.flow -> 'a t
(** Create a post-condition from a flow *)

val add_mergers : Ast.stmt list -> 'a t -> 'a t
(** [add_mergers m p] adds meet mergers [m] to post-condition [p] *)

val join : 'a t -> 'a t -> flow_join:('a Flow.flow -> 'a Flow.flow -> 'a Flow.flow) -> 'a t
(** Join two post-conditions *)

