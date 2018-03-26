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
    (* | E_var v when is_inttype v.vtyp ->
     *   assert false
     *
     * | E_unop(op, e) when is_inttype e.etyp ->
     *   assert false
     *
     * | E_binop(op, e1, e2) when is_inttype e1.etyp && is_inttype e2.etyp ->
     *   assert false *)

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
