(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for comparison operators. *)


open Framework.Domains
open Framework.Flow
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Universal.Ast
open Framework.Domains.Stateless
open Universal.Ast
open Ast
open Addr
open Operators

let name = "python.data_model.compare_ops"
let debug fmt = Debug.debug ~channel:name fmt


module Domain = struct

  (* check equality by inspecting types and instance addresses. Return
     None when comparison can not be determined. *)
  let is_equal ((e1: expr), (cls1: addr)) ((e2: expr), (cls2: addr)) : bool option =
    (* different type => not equal *)
    if compare_addr cls1 cls2 <> 0 then Some false
    else
      match ekind e1, ekind e2 with
      | E_addr addr1, E_addr addr2 ->
        (* different addresses => not equal *)
        if compare_addr addr1 addr2 <> 0 then
          Some false
        else
          (* same strong addresses => equal *)
        if not (Universal.Heap.Recency.is_weak addr1) || not (Universal.Heap.Recency.is_weak addr2) then
          Some true
        else
          (* cannot say anything with weak addresses *)
          None

      (* one of the arguments is not in the heap => not equal *)
      (* FIXME: is this reachable? *)
      | E_addr _, _ | _, E_addr _ -> Some false

      (* FIXME: this is sound, but certainly imprecise *)
      | _ -> None

  let is_empty e =
    match etyp e with
    | T_empty -> true
    | _ -> false

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, e1, e2) when is_comp_op op ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

          if is_empty e1 || is_empty e2 then
            oeval_singleton (Some (mk_binop e1 op e2 ~etyp:T_bool range), flow, [])
          else

            let op_fun, rop_fun =
              match op with
              | O_eq -> "__eq__", "__eq__"
              | O_ne -> "__ne__", "__ne__"
              | O_lt -> "__lt__", "__gt__"
              | O_le -> "__le__", "__ge__"
              | O_gt -> "__gt__", "__lt__"
              | O_ge -> "__ge__", "__le__"
              | _ -> assert false
            in

            let cls1 = Addr.classof @@ addr_of_expr e1 in
            let cls2 = Addr.classof @@ addr_of_expr e2 in

            man.eval ctx (mk_py_call (mk_py_addr_attr cls1 op_fun range) [e1; e2] range) flow |>
            eval_compose (fun cmp flow ->
                match etyp cmp with
                | T_py_not_implemented ->
                  (* FIXME: subclass priority check is not implemented *)
                  begin
                    match op with
                    | O_eq | O_ne ->
                      (* Apply default equality test *)
                      begin
                        match op, is_equal (e1, cls1) (e2, cls2) with
                        | O_eq, Some true -> oeval_singleton (Some (mk_true range), flow, [])
                        | O_eq, Some false -> oeval_singleton (Some (mk_false range), flow, [])
                        | O_ne, Some true -> oeval_singleton (Some (mk_false range), flow, [])
                        | O_ne, Some false -> oeval_singleton (Some (mk_true range), flow, [])
                        | _, None -> oeval_singleton (Some (mk_top T_bool range), flow, [])
                        | _ -> assert false
                      end
                    | _ ->
                      let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                      oeval_singleton (None, flow, [])
                  end
                | _ -> oeval_singleton (Some cmp, flow, [])
              )
        )

    | _ -> None


  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
