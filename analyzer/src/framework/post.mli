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
(** Post-conditions of statement transfer functions *)


val return : ?mergers:Ast.stmt list -> 'a Flow.flow -> 'a post option
(** Create a post-condition from a flow *)

val map : ('a Flow.flow -> 'b Flow.flow) -> 'a post option -> 'b post option
(** Map the flow of a post-condition *)

val add_mergers : Ast.stmt list -> 'a post option -> 'a post option
(** [add_mergers m p] adds meet mergers [m] to post-condition [p] *)

val join : 'a post option -> 'a post option -> fjoin:('a Flow.flow -> 'a Flow.flow -> 'a Flow.flow) -> 'a post option
(** Join two post-conditions *)

val bind :
  ?zone:Zone.t -> ('a, 't) Manager.manager -> Context.context ->
  ('e -> 'a Flow.flow -> 'a post option) -> ('e, 'a) Eval.eval -> 'a post option
