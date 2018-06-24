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


type 'a interface = 'a Domain.interface


module type DOMAIN =
sig

  val init : Ast.program -> ('a, unit) manager -> Context.context -> 'a flow -> (Context.context * 'a flow) option

  (** Abstract transfer function of statements. *)
  val exec_interface : Zone.t interface
  val exec: Zone.t -> Ast.stmt -> ('a, unit) manager -> Context.context -> 'a flow -> 'a Post.post option

  (** Abstract (symbolic) evaluation of expressions. *)
  val eval_interface : Zone.path interface
  val eval: Zone.path -> Ast.expr -> ('a, unit) manager -> Context.context -> 'a flow -> (Ast.expr, 'a) Eval.eval option

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

  let exec_interface = D.exec_interface
  let exec  = D.exec

  let eval_interface = D.eval_interface
  let eval = D.eval

  let ask = D.ask

end



let register_domain name modl =
  let module D = (val modl : DOMAIN) in
  let module SD = MakeStatefulDomain(D) in
  Domain.register_domain name (module SD)
