(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)



(** Analyzer - Central orchestrer of the analysis architecture. *)

module Make(Domain : Domain.DOMAIN) :
sig

  val init : Ast.program -> Context.context * Domain.t Flow.flow

  val exec : ?zone:Zone.t -> Ast.stmt -> Context.context -> Domain.t Flow.flow -> Domain.t Flow.flow

  val eval : ?zpath:Zone.path -> Ast.expr -> Context.context -> Domain.t Flow.flow -> (Ast.expr, Domain.t) Eval.t

  val ask : 'r Query.query -> Context.context -> Domain.t Flow.flow -> 'r option

  val manager : (Domain.t, Domain.t) Manager.manager

end

val mk_exec_of_zone_list :
  Zone.t list ->
  (Zone.t -> Ast.stmt -> ('a, 't) Manager.manager -> 'b -> 'a Flow.flow -> 'a Post.t option) ->
  (Ast.stmt -> ('a, 't) Manager.manager -> 'b -> 'a Flow.flow -> 'a Post.t option)


val mk_eval_of_zone_path_list :
  Zone.path list ->
  (Zone.path -> Ast.expr -> ('a, 't) Manager.manager -> 'b -> 'a Flow.flow -> (Ast.expr, 'a) Eval.t option) ->
  (Ast.expr -> ('a, 't) Manager.manager -> 'b -> 'a Flow.flow -> (Ast.expr, 'a) Eval.t option)
