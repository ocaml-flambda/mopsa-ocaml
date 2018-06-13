(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Domains without lattice data structure. *)

open Flow
open Lattice
open Manager


(*==========================================================================*)
                  (** {2 Domain signature} *)
(*==========================================================================*)


module type DOMAIN =
sig

  val init : Ast.program -> ('a, unit) manager -> Context.context -> 'a flow -> (Context.context * 'a flow) option

  (** Abstract transfer function of statements. *)
  val import_exec : Zone.t list
  val export_exec : Zone.t list
  val exec: Zone.t -> Ast.stmt -> ('a, unit) manager -> Context.context -> 'a flow -> 'a Post.t option

  (** Abstract (symbolic) evaluation of expressions. *)
  val import_eval : Zone.path list
  val export_eval : Zone.path list
  val eval: Zone.path -> Ast.expr -> ('a, unit) manager -> Context.context -> 'a flow -> (Ast.expr, 'a) Eval.t option

  (** Handler of generic queries. *)
  val ask: 'r Query.query -> ('a, unit) manager -> Context.context -> 'a flow -> 'r option


end

(** Create a stateful domain from a stateless one. *)
module MakeStatefulDomain(D: DOMAIN) : Domain.DOMAIN =
struct

  type t = unit
  let bottom = ()
  let top = ()
  let is_bottom _ = false
  let is_top _ = true
  let leq _ _ = true
  let unify _ a1 a2 = (a1, a2)
  let join _ _ = top
  let meet _ _ = top
  let widening _ _ _ = top
  let print _ _ = ()


  let init = D.init

  let import_exec = D.import_exec
  let export_exec = D.export_exec
  let exec  = D.exec

  let import_eval = D.import_eval
  let export_eval = D.export_eval
  let eval = D.eval

  let ask = D.ask

end



let register_domain name modl =
  let module D = (val modl : DOMAIN) in
  let module SD = MakeStatefulDomain(D) in
  Domain.register_domain name (module SD)
