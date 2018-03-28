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

  let init prog man flow = flow

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
      Eval.re_eval_singleton man ctx (Some e', flow, [])

    | _ -> None

  let exec stmt man ctx flow = None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
