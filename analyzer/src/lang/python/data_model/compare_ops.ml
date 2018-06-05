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
  let is_equal (e1: expr) (e2: expr) : bool option =
    let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
    let cls1 = Addr.class_of_object o1 and cls2 = Addr.class_of_object o2 in
    (* different type => not equal *)
    if compare_py_object cls1 cls2 <> 0 then Some false
    else (* different addresses => not equal *)
    if compare_py_object o1 o2 <> 0 then
      Some false
    else (* same strong addresses => equal *)
    if not (Addr.is_weak o1) || not (Addr.is_weak o2) then
      Some true
    else (* cannot say anything with weak addresses *)
      None

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, e1, e2) when is_comp_op op ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

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

          let cls1 = Addr.class_of_object @@ object_of_expr e1 in

          man.eval ctx (mk_py_call (mk_py_object_attr cls1 op_fun range) [e1; e2] range) flow |>
          eval_compose (fun cmp flow ->
              if Addr.is_not_implemented cmp then
                (* FIXME: subclass priority check is not implemented *)
                begin
                  match op with
                  | O_eq | O_ne ->
                    (* Apply default equality test *)
                    begin
                      match op, is_equal e1 e2 with
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
              else
                oeval_singleton (Some cmp, flow, [])
            )
        )

    | _ -> None


  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
