(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


type 'a post = {
  flow : 'a Flow.flow;
  mergers : Ast.stmt list;
}

type 'a t
(** Post-condition of [exec] transfer functions *)



val return : ?mergers:Ast.stmt list -> 'a Flow.flow -> 'a t
(** Create a post-condition from a flow *)

val map : ('a Flow.flow -> 'b Flow.flow) -> 'a t -> 'b t
(** Map the flow of a post-condition *)

val add_mergers : Ast.stmt list -> 'a t -> 'a t
(** [add_mergers m p] adds meet mergers [m] to post-condition [p] *)

val join : 'a t -> 'a t -> fjoin:('a Flow.flow -> 'a Flow.flow -> 'a Flow.flow) -> 'a t
(** Join two post-conditions *)

val bind :
    Ast.expr ->
    ?zpath:Zone.path ->
    ('a, 't) Manager.manager -> Context.context -> 'a Flow.flow ->
    (Ast.expr -> 'a Flow.flow -> 'a t) ->
    'a t
