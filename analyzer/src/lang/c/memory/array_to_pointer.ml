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
open Framework.Flow
open Framework.Utils
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

  let init prog man ctx flow = ctx, flow

  let exec (stmt : stmt) (man : ('a, unit) manager) ctx (flow : 'a flow) : 'a flow option = None

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_array_subscript(arr, idx) ->
      debug "array subscript to deref";
      let exp' = {exp with ekind = E_c_deref(mk_binop arr O_plus idx exp.erange ~etyp: arr.etyp)} in
      re_eval_singleton (Some exp', flow, []) man ctx

    | E_constant(C_c_string (s, _)) ->
      let table = Program.find_string_table ctx in
      let v = Program.StringTable.find s table in
      let v = mk_var v exp.erange in
      re_eval_singleton (Some v, flow, []) man ctx


    | _ -> None



  let ask request man ctx flow = None


end

let setup () =
  register_domain name (module Domain)
