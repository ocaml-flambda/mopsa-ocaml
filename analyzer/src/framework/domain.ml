(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Signature of an abstract domain.

   This is the low-level domain signature that gives full access to the
   overall flow abstraction and analysis manager.
*)

open Lattice
open Flow
open Manager
open Eval


module type DOMAIN =
sig

  include Lattice.LATTICE

  val init : Ast.program -> ('a, t) manager -> Context.context -> 'a flow -> (Context.context * 'a flow) option

  (** Abstract transfer function of statements. *)
  val import_exec : Zone.t list
  val export_exec : Zone.t list
  val exec: Zone.t -> Ast.stmt -> ('a, t) manager -> Context.context -> 'a flow -> 'a Post.t option

  (** Abstract (symbolic) evaluation of expressions. *)
  val import_eval : Zone.path list
  val export_eval : Zone.path list
  val eval: Zone.path -> Ast.expr -> ('a, t) manager -> Context.context -> 'a flow -> (Ast.expr, 'a) Eval.t option

  (** Handler of generic queries. *)
  val ask: 'r Query.query -> ('a, t) manager -> Context.context -> 'a flow -> 'r option
end


let domains : (string * (module DOMAIN)) list ref = ref []
let register_domain name modl = domains := (name, modl) :: !domains
let find_domain name = List.assoc name !domains
