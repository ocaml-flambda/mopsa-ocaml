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

type 'a interface = {
  export : 'a list;
  import : 'a list;
}

module type DOMAIN =
sig

  include Lattice.LATTICE

  val init : Ast.program -> ('a, t) manager -> Context.context -> 'a flow -> (Context.context * 'a flow) option

  (** Abstract transfer function of statements. *)
  val exec_interface : Zone.t interface
  val exec: Zone.t -> Ast.stmt -> ('a, t) manager -> Context.context -> 'a flow -> 'a Post.post option

  (** Abstract (symbolic) evaluation of expressions. *)
  val eval_interface : Zone.path interface
  val eval: Zone.path -> Ast.expr -> ('a, t) manager -> Context.context -> 'a flow -> (Ast.expr, 'a) Eval.eval option

  (** Handler of generic queries. *)
  val ask: 'r Query.query -> ('a, t) manager -> Context.context -> 'a flow -> 'r option
end


let domains : (string * (module DOMAIN)) list ref = ref []
let register_domain name modl = domains := (name, modl) :: !domains
let find_domain name = List.assoc name !domains
