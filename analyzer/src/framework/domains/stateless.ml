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
open Eval
    

(*==========================================================================*)
                  (** {2 Domain signature} *)
(*==========================================================================*)


module type DOMAIN =
sig

  val init :
    ('a, unit) manager -> Context.context -> Ast.program -> 'a flow ->
    Context.context * 'a flow

  (** Abstract transfer function of statements. *)
  val exec:
    ('a, unit) manager -> Context.context -> Ast.stmt -> 'a flow ->
    'a flow option

  (** Abstract (symbolic) evaluation of expressions. *)
  val eval:
    ('a, unit) manager -> Context.context -> Ast.expr -> 'a flow ->
    (Ast.expr, 'a) evals option

  (** Handler of generic queries. *)
  val ask:
    ('a, unit) manager -> Context.context -> 'r Query.query -> 'a flow ->
    'r option

end

(** Creates a low-level domain from a high-level one. *)
module MakeStatefulDomain(Domain: DOMAIN) : Stateful.DOMAIN =
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


  let init = Domain.init
  let exec  = Domain.exec
  let eval = Domain.eval
  let ask = Domain.ask

end



let register_domain name modl =
  let module D = (val modl : DOMAIN) in
  let module GD = MakeStatefulDomain(D) in
  Stateful.register_domain name (module GD)

