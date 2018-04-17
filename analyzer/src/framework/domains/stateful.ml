(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Domains with global view on the full abstraction.

   This is the low-level domain signature that gives full access to the
   overall flow abstraction and analysis manager.
*)

open Lattice
open Flow
open Manager
open Eval


(*==========================================================================*)
                        (** {2 Standlone domains} *)
(*==========================================================================*)

(** Low level abstract domain. *)
module type DOMAIN =
sig

  include Lattice.LATTICE

  val init :
    ('a, t) manager -> Context.context -> Ast.program -> 'a flow ->
    Context.context * 'a flow

  (** Abstract transfer function of statements. *)
  val exec:
    ('a, t) manager -> Context.context -> Ast.stmt -> 'a flow ->
    'a flow option

  (** Abstract (symbolic) evaluation of expressions. *)
  val eval:
    ('a, t) manager -> Context.context -> Ast.expr -> 'a flow ->
    (Ast.expr, 'a) evals option

  (** Handler of generic queries. *)
  val ask:
    ('a, t) manager -> Context.context -> 'r Query.query -> 'a flow ->
    'r option
end


(** Low level functor abstract domain. *)
module type FUNCTOR = functor(_ : DOMAIN) -> DOMAIN


module EmptyDomain : DOMAIN =
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
  let init _ ctx _ x = ctx, x
  let exec _ _ _ _ = None
  let eval _ _ _ _ = None
  let ask _ _ _ _ = None
end

let domains : (string * (module DOMAIN)) list ref = ref []
let register_domain name modl = domains := (name, modl) :: !domains
let find_domain name = List.assoc name !domains

let () = register_domain "empty" (module EmptyDomain)


let functors : (string * (module FUNCTOR)) list ref = ref []

let register_functor name modl =
  functors := (name, modl) :: !functors

let find_functor name = List.assoc name !functors

let mk_lattice_manager (type a) (domain: (module DOMAIN with type t = a)) : a lattice_manager =
  let module Domain = (val domain) in
  {
    bottom = Domain.bottom;
    top = Domain.top;
    is_bottom = Domain.is_bottom;
    is_top = Domain.is_top;
    leq = Domain.leq;
    join = Domain.join;
    meet = Domain.meet;
    widening = Domain.widening;
    print = Domain.print;
  }
