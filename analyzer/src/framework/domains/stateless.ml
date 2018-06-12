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

  include Domain.DOMAIN with type t := unit

end

(** Create a stateful domain from a stateless one. *)
module MakeStatefulDomain(D: DOMAIN) : Domain.DOMAIN =
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


  let init = D.init

  let import_exec = D.import_exec
  let export_exec = D.export_exec
  let exec  = D.exec

  let import_eval = D.import_eval
  let export_eval = D.export_eval
  let eval = D.eval

  let ask = D.ask

end



let register_domain name modl =
  let module D = (val modl : DOMAIN) in
  let module SD = MakeStatefulDomain(D) in
  Domain.register_domain name (module SD)

let return x = Some x
let fail = None
