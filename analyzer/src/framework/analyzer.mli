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

  val exec : ?zone:Zone.zone -> Ast.stmt -> Domain.t flow -> Domain.t flow

  val eval : ?zone:(Zone.zone * Zone.zone) -> Ast.expr -> Domain.t flow -> (Domain.t, Ast.expr) evl

  val ask : 'r Query.query -> Domain.t Flow.flow -> 'r

  val man : (Domain.t, Domain.t) man

  val output_actions : unit -> unit

end
