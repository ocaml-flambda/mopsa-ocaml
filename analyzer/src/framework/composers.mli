(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Transfer function composers *)

val map :
    ('e -> 'a Flow.flow -> ('x, 'a) Eval.t option) ->
    ('e, 'a) Eval.t ->
    ('x, 'a) Eval.t option
(** [map f evl] applies [f] on cases of [evl] and joins the results *)

val post :
  ('a, 't) Manager.manager -> Context.context ->
  ('e -> 'a Flow.flow -> 'a Post.t option) ->
  ('e, 'a) Eval.t ->
  'a Post.t option
(** [post man ctx f evl] computes of the post-condition of [f] over evaluations [evl] *)

val map_eval:
  Zone.path -> Ast.expr -> ('a, 't) Manager.manager -> Context.context -> 'a Flow.flow ->
  (Ast.expr -> 'a Flow.flow -> ('e, 'a) Eval.t option) ->
  ('e, 'a) Eval.t option
(** [map_eval zpath e man ctx flow f] evaluates expression [e] using
   manager [man] and applies the evaluation function [f] on all its cases *)

val post_eval:
  Zone.path -> Ast.expr -> ('a, 't) Manager.manager -> Context.context -> 'a Flow.flow ->
  (Ast.expr -> 'a Flow.flow -> 'a Post.t option) ->
  'a Post.t option
(** [post_eval zpath e man ctx flow f] evaluates expression [e] using
   manager [man] and computes the post-condition [f] on all its cases *)
