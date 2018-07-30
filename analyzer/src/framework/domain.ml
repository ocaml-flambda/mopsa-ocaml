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

open Manager
open Eval

type 'a interface = {
  export : 'a list;
  import : 'a list;
}

module type DOMAIN =
sig

  include Lattice.LATTICE

  val init : Ast.program -> ('a, t) man -> 'a flow -> 'a flow option

  val exec_interface : Zone.t interface
  val eval_interface : (Zone.t * Zone.t) interface

  val exec : Zone.t -> Ast.stmt -> ('a, t) man -> 'a flow -> 'a Post.post option
  val eval : (Zone.t * Zone.t) -> Ast.expr -> ('a, t) man -> 'a flow -> ('a, Ast.expr) evl option
  val ask  : 'r Query.query -> ('a, t) man -> 'a flow -> 'r option
end

let domains : (string * (module DOMAIN)) list ref = ref []

let register name dom =
  domains := (name, dom) :: !domains

let find name =
  List.assoc name !domains
