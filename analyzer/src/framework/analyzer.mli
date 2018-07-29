(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Analyzer - Central orchestrer of the analysis architecture. *)

open Manager

module Make(Domain : Domain.DOMAIN) :
sig

  val init : Ast.program -> Domain.t flow

  val exec : ?zone:Zone.t -> Ast.stmt -> Domain.t flow -> Domain.t flow

  val eval : ?zone:(Zone.t * Zone.t) -> Ast.expr -> Domain.t flow -> (Domain.t, Ast.expr) evl

  val ask : 'r Query.query -> Domain.t Flow.flow -> 'r

  val man : (Domain.t, Domain.t) man

end

val mk_exec_of_zone_list :
  Zone.t list ->
  (Zone.t -> Ast.stmt -> ('a, 't) man -> 'a flow -> 'a Post.post option) ->
  (Ast.stmt -> ('a, 't) man -> 'a flow -> 'a Post.post option)


val mk_eval_of_zone_list :
  (Zone.t * Zone.t) list ->
  ((Zone.t * Zone.t) -> Ast.expr -> ('a, 't) man -> 'a flow -> ('a, Ast.expr) evl option) ->
  (Ast.expr -> ('a, 't) man -> 'a flow -> ('a, Ast.expr) evl option)
