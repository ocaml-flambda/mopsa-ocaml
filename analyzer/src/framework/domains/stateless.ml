(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Stateless domains without lattice data structure. *)

open Essentials

module type S =
sig

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


  let init = D.init

  let exec_interface = {
    export = [];
    import = [];
  }

  let eval_interface = {
    export = [];
    import = [];
  }
  
  let exec zone = D.exec
  let eval zone = D.eval
  let ask = D.ask

end



let register_domain name modl =
  let module D = Make(val modl : S) in
  Domain.register name (module D)
