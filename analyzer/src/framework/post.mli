(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


type reduction_channel
(** Post-condition reduction channels *)

type 'a t = {
  flow      : 'a Flow.flow;            (** Post-condition flow *)
  channels  : reduction_channel list;  (** Published post-condition reductions *)
  mergers   : Ast.stmt list;           (** Meet mergers *)
}
(** Post-condition of [exec] transfer functions *)

val of_flow : ?channels:reduction_channel list -> ?mergers:Ast.stmt list -> 'a Flow.flow -> 'a t
(** Create a post-condition from a flow *)

val join : 'a t -> 'a t -> flow_join:('a Flow.flow -> 'a Flow.flow -> 'a Flow.flow) -> 'a t
(** Join two post-conditions *)
