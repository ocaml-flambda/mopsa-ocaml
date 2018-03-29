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

  let init prog man flow =
    match prog.prog_kind with
    | C_program(globals, _) ->
      let range = mk_fresh_range () in
      List.fold_left (fun flow (v, init) ->
          if not (is_c_int_type v.vtyp) then flow
          else
            let e =
              match init with
              | None -> mk_zero range
              | Some (C_init_expr e) -> e
              | _ -> assert false
            in
            man.exec (mk_assign (mk_var v range) e range) Framework.Context.empty flow
        ) flow globals

    | _ -> flow

  let eval exp man ctx flow =
    match ekind exp with
    | E_unop(op, e) when is_c_int_type e.etyp ->
      Eval.compose_eval e
        (fun e flow ->
           let e' = {e with etyp = T_int} in
           let exp' = {exp with ekind = E_unop(op, e')} in
           Eval.singleton (Some exp', flow, [])
        )
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow

    | E_binop(op, e1, e2) when is_c_int_type e1.etyp && is_c_int_type e2.etyp ->
      Eval.compose_eval_list [e1; e2]
        (fun el flow ->
           let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
           let e1 = {e1 with etyp = T_int} in
           let e2 = {e2 with etyp = T_int} in
           let exp' = {exp with ekind = E_binop(op, e1, e2)} in
           Eval.singleton (Some exp', flow, [])
        )
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow


    | E_c_cast(e', _) ->
      debug "cast";
      Eval.re_eval_singleton man ctx (Some e', flow, [])

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
      Exec.return flow

    | _ -> None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
