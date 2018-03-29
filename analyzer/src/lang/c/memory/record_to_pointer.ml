(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Transformer of struct/union into pointers arithmetics. *)

open Framework.Ast
open Framework.Pp
open Framework.Domains.Stateless
open Framework.Manager
open Framework.Visitor
open Framework.Domains
open Framework.Flow
open Universal.Ast
open Ast


let name = "c.memory.record_to_pointer"
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
    | S_c_local_declaration(v, None) when is_c_record_type v.vtyp ->
      Exec.return flow

    | S_c_local_declaration(v, Some init) when is_c_record_type v.vtyp ->
      panic "Initialization of structs/unions not supported"
        
    | _ -> None

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_member_access(r, idx, f) ->
      let exp' = {exp with ekind = E_c_arrow_access(mk_c_address_of r r.erange, idx, f)} in
      Eval.re_eval_singleton man ctx (Some exp', flow, [])

        
    | _ -> None



  let ask request man ctx flow = None


end

let setup () =
  register_domain name (module Domain)
