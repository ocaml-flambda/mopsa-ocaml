(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


type reduction_channel
(** Post-condition reduction channels *)

type 'a post = {
  flow      : 'a Flow.flow;            (** Post-condition flow *)
  channels  : reduction_channel list;  (** Published post-condition reductions *)
  mergers   : Ast.stmt list;           (** Meet mergers *)
}
(** Post-condition of [exec] transfer functions *)

val singleton : ?channels:reduction_channel list -> ?mergers:Ast.stmt list -> 'a Flow.flow -> 'a post
(** Singleton post-condition *)

val join : 'a post -> 'a post -> flow_join:('a Flow.flow -> 'a Flow.flow -> 'a Flow.flow) -> 'a post
(** Join two post-conditions *)
