(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Stateless domains are domains without a lattice structure. Only
   transfer functions are defined. *)

open Essentials

module type S =
sig

  val name     : string
  val id       : unit domain
  val identify : 'b domain -> (unit, 'b) eq option
  val exec_interface : Zone.zone interface
  val eval_interface : (Zone.zone * Zone.zone) interface
  val init : Ast.program -> ('a, unit) man -> 'a flow -> 'a flow option
  val exec : Ast.stmt -> ('a, unit) man -> 'a flow -> 'a post option
  val eval : Ast.expr -> ('a, unit) man -> 'a flow -> ('a, Ast.expr) evl option
  val ask  : 'r Query.query -> ('a, unit) man -> 'a flow -> 'r option

end

(** Create a stateful domain from a stateless one. *)
module Make(D: S) : Domain.DOMAIN =
struct

  type t = unit
  let bottom = ()
  let top = ()
  let is_bottom _ = false
  let subset _ _ = true
  let join _ _ _ = top
  let meet _ _ _ = top
  let widen _ _ _ = top
  let print _ _ = ()

  let name = D.name
  let id = D.id
  let identify = D.identify

  let init = D.init

  let exec_interface = D.exec_interface
  let eval_interface = D.eval_interface

  let exec zone = D.exec
  let eval zone = D.eval
  let ask = D.ask

end



let register_domain modl =
  let module M = (val modl : S) in
  let module D = Make(M) in
  Domain.register_domain (module D)
