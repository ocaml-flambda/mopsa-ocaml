(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Unification domain.

   Signature for domains requiring a unification step of a sub-domain before
   every call of binary operators (⊆, ∪ and ∩).
*)

open Lattice
open Flow
open Manager
open Context
open Eval
open Query


(** Abstract domain signature. *)
module type DOMAIN =
sig

  include Stateful.DOMAIN

  val unify :
    ('a, 'b) manager -> context ->
    t * 'b -> t * 'b ->
    (t * 'b) * (t * 'b)

  val exec:
    ('a, t) manager -> ('a, 'b) manager -> Context.context -> Ast.stmt -> 'a flow ->
    'a flow option

  val eval:
    ('a, t) manager -> ('a, 'b) manager -> Context.context -> Ast.expr -> 'a flow ->
    (Ast.expr, 'a) evals option

  (** Handler of generic queries. *)
  val ask:
    ('a, t) manager -> ('a, 'b) manager -> Context.context -> 'r Query.query -> 'a flow ->
    'r option

end



let domains : (string * (module DOMAIN)) list ref = ref []
let register_domain name modl = domains := (name, modl) :: !domains
let find_domain name = List.assoc name !domains

let return x = Some x
let fail = None
