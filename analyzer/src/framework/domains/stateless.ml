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

let fail = Global.fail
let return = Global.return
let oflow_extract = Global.oflow_extract
let oflow_extract_dfl = Global.oflow_extract_dfl
let oflow_map = Global.oflow_map
let oflow_merge = Global.oflow_merge
let oeval_singleton = Global.oeval_singleton
let oeval_map = Global.oeval_map
let oeval_join = Global.oeval_join
let oeval_meet = Global.oeval_meet


(*==========================================================================*)
                  (** {2 Domain signature} *)
(*==========================================================================*)


module type DOMAIN =
sig

  val init :
    Ast.program -> ('a, unit) manager -> 'a flow ->
    'a flow

  (** Abstract transfer function of statements. *)
  val exec:
    Ast.stmt -> ('a, unit) manager -> Context.context -> 'a flow ->
    'a flow option

  (** Abstract (symbolic) evaluation of expressions. *)
  val eval:
    Ast.expr -> ('a, unit) manager -> Context.context -> 'a flow ->
    (Ast.expr, 'a) evals option

  (** Handler of generic queries. *)
  val ask:
    'r Query.query -> ('a, unit) manager -> Context.context -> 'a flow ->
    'r option

end

(** Creates a low-level domain from a high-level one. *)
module MakeGlobalDomain(Domain: DOMAIN) : Global.DOMAIN =
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
  let module GD = MakeGlobalDomain(D) in
  Global.register_domain name (module GD)


module type STACK_DOMAIN =
  functor(Sub: Global.DOMAIN) ->
  sig

    val init :
       Ast.program -> ('a, unit) manager -> ('a, Sub.t) manager ->'a flow ->
      'a flow

    (** Abstract transfer function of statements. *)
    val exec:
      Ast.stmt -> ('a, unit) manager -> ('a, Sub.t) manager ->
      Context.context -> 'a flow ->
      'a flow option

    (** Abstract (symbolic) evaluation of expressions. *)
    val eval:
      Ast.expr -> ('a, unit) manager -> ('a, Sub.t) manager ->
      Context.context -> 'a flow ->
      (Ast.expr, 'a) evals option

    (** Handler of generic queries. *)
    val ask:
      'r Query.query -> ('a, unit) manager -> ('a, Sub.t) manager ->
      Context.context -> 'a flow ->
      'r option

  end


module MakeGlobalStackDomain =
  functor(Stack: STACK_DOMAIN) ->
  functor(Sub: Global.DOMAIN) ->
  struct

    module Domain = Stack(Sub)

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



let register_stack_domain name modl =
  let module D = (val modl : STACK_DOMAIN) in
  let module GD = MakeGlobalStackDomain(D) in
  Global.register_stack_domain name (module GD)
