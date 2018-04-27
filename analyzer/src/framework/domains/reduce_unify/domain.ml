(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Domains supporting reduction and unification.*)

open Lattice
open Flow
open Manager
open Eval
open Query
open Context
open Reduce.Domain

(** Abstract domain signature. *)
module type DOMAIN = functor(SubDomain: Stateful.DOMAIN) ->
sig

  include Lattice.LATTICE

  val init :
    ('a, t) manager ->
    Context.context ->
    Ast.program -> 'a flow ->
    Context.context * 'a flow

  val unify :
    context ->
    t * SubDomain.t  -> t * SubDomain.t ->
    (t * SubDomain.t) * (t * SubDomain.t)

  (** Abstract transfer function of statements. *)
  val exec:
    ('a, t) manager -> ('a, SubDomain.t) manager ->
    Context.context ->
    Ast.stmt -> 'a flow ->
    'a rflow option

  (** Refine a post condition by exploiting information from a reduction channel *)
  val refine:
    ('a, t) manager -> ('a, SubDomain.t) manager ->
    Context.context ->
    channel -> 'a flow ->
    'a rflow option

  (** Evaluation of expressions. *)
  val eval:
    ('a, t) manager -> ('a, SubDomain.t) manager ->
    Context.context ->
    Ast.expr -> 'a flow ->
    'a revals option

  (** Query handler. *)
  val ask:
    ('a, t) manager -> ('a, SubDomain.t) manager ->
    Context.context ->
    'r Query.query -> 'a flow ->
    'r option
end

let domains : (string * (module DOMAIN)) list ref = ref []
let register_domain name modl = domains := (name, modl) :: !domains
let find_domain name = List.assoc name !domains

let return = return
let fail = fail

let eval_to_rexec = eval_to_rexec
let eval_to_orexec = eval_to_orexec
