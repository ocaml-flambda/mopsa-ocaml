(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Evaluations of expressions *)
type ('e, 'a) case = ('e, 'a) Manager.eval_case
type ('e, 'a) t
(** Evaluations *)

val singleton : 'e option -> ?cleaners:Ast.stmt list -> 'a Flow.flow -> ('e, 'a) t
(** Evaluation singleton *)

val join : ('e, 'a) t -> ('e, 'a) t -> ('e, 'a) t
(** Compute the union of two evaluations *)

val add_cleaners : Ast.stmt list -> ('e, 'a) t -> ('e, 'a) t
(** Add cleaners to an evaluation *)

val map:
    ('e -> 'a Flow.flow -> ('f, 'a) case) ->
    ('e, 'a) t ->
    ('f, 'a) t
(** [map f evls] applies the evaluation function [f] on each
   case of [evls] and joins the results *)

val fold: ('b -> ('e, 'a) case -> 'b) -> 'b -> ('e, 'a) t -> 'b option

val bind :
  Ast.expr -> ('a, 't) Manager.manager -> ?zpath:Zone.path -> Context.context -> 'a Flow.flow ->
  (Ast.expr -> 'a Flow.flow -> ('e, 'a) t) ->
  ('e, 'a) t

val bind_list :
  Ast.expr list -> ('a, 't) Manager.manager -> ?zpath:Zone.path -> Context.context -> 'a Flow.flow ->
  (Ast.expr list -> 'a Flow.flow -> ('e, 'a) t) ->
  ('e, 'a) t
