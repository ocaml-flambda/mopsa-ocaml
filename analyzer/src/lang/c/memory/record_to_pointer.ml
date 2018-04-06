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
open Framework.Utils
open Universal.Ast
open Ast


let name = "c.memory.record_to_pointer"
let debug fmt = Debug.debug ~channel:name fmt



(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain = struct

  let get_nth_field r n =
    match remove_typedef r.vtyp |> remove_qual with
    | T_c_record{c_record_kind = C_struct; c_record_fields} -> List.nth c_record_fields n
    | _ -> assert false

  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)

  let init prog man flow = flow

  let exec (stmt : stmt) (man : ('a, unit) manager) ctx (flow : 'a flow) : 'a flow option =
    let range = stmt.srange in
    match skind stmt with
    | S_c_local_declaration(r, None) when is_c_record_type r.vtyp ->
      return flow

    | S_c_local_declaration(r, Some init) when is_c_struct_type r.vtyp ->
      let v = mk_var r range in
      begin
        match init with
        | C_init_list(l, None) ->
          let rec aux flow i = function
            | [] -> flow
            | C_init_expr e :: tl ->
              let field = get_nth_field r i in
              let flow = man.exec
                  (mk_assign (mk_c_member_access v field range) e range) ctx flow
              in
              aux flow (i + 1) tl

            | _ -> assert false
          in
          aux flow 0 l |>
          return

        | _ -> assert false
      end

    | _ -> None

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_member_access(r, idx, f) ->
      let exp' = {exp with ekind = E_c_arrow_access(mk_c_address_of r r.erange, idx, f)} in
      re_eval_singleton (Some exp', flow, []) man ctx


    | _ -> None



  let ask request man ctx flow = None


end

let setup () =
  register_domain name (module Domain)
