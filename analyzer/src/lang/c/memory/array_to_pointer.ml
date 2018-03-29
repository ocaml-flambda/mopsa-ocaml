(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Arrays transformer into pointers arithmetics. *)

open Framework.Ast
open Framework.Pp
open Framework.Domains.Stateless
open Framework.Manager
open Framework.Visitor
open Framework.Domains
open Framework.Flow
open Universal.Ast
open Ast


let name = "c.memory.array_to_pointer"
let debug fmt = Debug.debug ~channel:name fmt



(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain = struct


  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)


  let init prog man flow = flow

  let exec (stmt : stmt) (man : ('a, unit) manager) ctx (flow : 'a flow) : 'a flow option =
    match skind stmt with
    | _ -> None


  let eval exp man ctx flow =
    match ekind exp with
    | E_c_array_subscript(arr, idx) ->
      let exp' = {exp with ekind = E_c_deref(mk_binop arr O_plus idx exp.erange ~etyp:(T_c_pointer(exp.etyp)))} in
      Eval.re_eval_singleton man ctx (Some exp', flow, [])

    | _ -> None



  let ask request man ctx flow = None


end

let setup () =
  register_domain name (module Domain)
