(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Machine representation of C integers. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Utils
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.memory.machine_integers"
let debug fmt = Debug.debug ~channel:name fmt


(** Abstract domain. *)
module Domain =
struct

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init prog man flow = flow

  let eval exp man ctx flow =
    match ekind exp with
    | E_constant(C_c_character c) ->
      re_eval_singleton (Some (mk_int (int_of_char c) exp.erange), flow, []) man ctx

    | E_unop(op, e) when is_c_int_type e.etyp ->
      man.eval e ctx flow |>
      eval_compose
        (fun e flow ->
           let e' = {e with etyp = T_int} in
           let exp' = {exp with ekind = E_unop(op, e')} in
           oeval_singleton (Some exp', flow, [])
        )

    | E_binop(op, e1, e2) when is_c_int_type e1.etyp && is_c_int_type e2.etyp ->
      man_eval_list [e1; e2] man ctx flow |>
      oeval_compose
        (fun el flow ->
           let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
           let e1 = {e1 with etyp = T_int} in
           let e2 = {e2 with etyp = T_int} in
           let exp' = {exp with ekind = E_binop(op, e1, e2)} in
           oeval_singleton (Some exp', flow, [])
        )

    | E_c_cast(e', _) ->
      debug "cast";
      re_eval_singleton (Some e', flow, []) man ctx

    | _ -> None

  let exec stmt man ctx flow =
    match skind stmt with
    | S_c_local_declaration(v, init) when is_c_int_type v.vtyp ->
      let flow =
        match init with
        | None -> flow
        | Some (C_init_expr e) -> man.exec (mk_assign (mk_var v stmt.srange) e stmt.srange) ctx flow
        | Some (Ast.C_init_list (_,_)) -> assert false
        | Some (Ast.C_init_implicit _) -> assert false
      in
      return flow

    | _ -> None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
