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

  val exec : Zone.t -> Ast.stmt -> Context.context -> Domain.t Flow.flow -> Domain.t Flow.flow

  val eval : Zone.path -> Ast.expr -> Context.context -> Domain.t Flow.flow -> (Ast.expr, Domain.t) Eval.evals

  val ask : 'r Query.query -> Context.context -> Domain.t Flow.flow -> 'r option

  val manager : (Domain.t, Domain.t) Manager.manager
      
end
