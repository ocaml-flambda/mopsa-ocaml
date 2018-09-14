(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Post-conditions of a domain's [exec] transfer function *)

open Manager

type channel = ..
(** Reduction channels *)

type z_stmt = Zone.zone * Ast.stmt
(** zoned statement *)

type 'a post = {
  flow : 'a flow;
  mergers : z_stmt list;
  channels : channel list;
}
(** Post-conditions *)

val of_flow : 'a flow -> 'a post
(** [of_flow flow] returns a post-condition from a flow, without
   mergers and channels *)

val return : 'a flow -> 'a post option
(** [of_flow flow] returns a post-condition option from a flow,
   without mergers and channels *)

val add_mergers_to_top : Ast.stmt list -> 'a post -> 'a post
(** [add_mergers m p] adds meet mergers (here a list of statement) [m]
   to post-condition [p], this statements will be executed on zone
   Z_top *)

val add_mergers : z_stmt list -> 'a post -> 'a post
(** [add_mergers m p] adds meet mergers [m] to post-condition [p] *)

val add_channels : channel list -> 'a post -> 'a post
(** [add_channels ch p] adds reduction channels [ch] to post-condition [p] *)

val map_flow : ('a flow -> 'a flow) -> 'a post -> 'a post
(** [map_flow f p] applies [f] on the underlying flow in [p]*)

val join : ('a, _) man -> 'a post -> 'a post -> 'a post
(** Join two post-conditions *)

val bind :
  ?zone:Zone.zone -> ('a, _) man ->
  ('e -> 'a flow -> 'a post) -> ('a, 'e) evl -> 'a post

val assume :
  Ast.expr -> ?zone:Zone.zone -> ('a, _) man ->
  fthen:('a Flow.flow -> 'a post) ->
  felse:('a Flow.flow -> 'a post) ->
  ?fboth:('a Flow.flow -> 'a Flow.flow -> 'a post) ->
  ?fnone:('a Flow.flow -> 'a post) ->
  'a flow ->
  'a post

val switch :
  ((Ast.expr * bool) list * ('a flow -> 'a post)) list ->
  ?zone:Zone.zone ->
  ('a, 'b) man -> 'a flow ->
  'a post

val print : ('a, _) man -> Format.formatter -> 'a post -> unit
