(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for comparison operators. *)


open Framework.Essentials
open Universal.Ast
open Ast
open Addr
open Operators


module Domain = struct

  type _ domain += D_python_data_model_compare_ops : unit domain

  let id = D_python_data_model_compare_ops
  let name = "python.data_model.compare_ops"
  let identify : type a. a domain -> (unit, a) eq option = function
    | D_python_data_model_compare_ops -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [Framework.Zone.Z_any, Framework.Zone.Z_any]; import = []}

  let init _ _ flow = Some flow


  (* check equality by inspecting types and instance addresses. Return
     None when comparison can not be determined. *)
  (* let is_equal (e1: expr) (e2: expr) : bool option =
   *   let o1 = object_of_expr e1 and o2 = object_of_expr e2 in
   *   let cls1 = Addr.class_of_object o1 and cls2 = Addr.class_of_object o2 in
   *   (\* different type => not equal *\)
   *   if compare_py_object cls1 cls2 <> 0 then Some false
   *   else (\* different addresses => not equal *\)
   *     if compare_py_object o1 o2 <> 0 then
   *       Some false
   *     else (\* same strong addresses => equal *\)
   *       if not (Addr.is_weak o1) || not (Addr.is_weak o2) then
   *         Some true
   *       else (\* cannot say anything with weak addresses *\)
   *         None *)

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, e1, e2) when is_comp_op op && is_py_expr e1 && is_py_expr e2 ->
       debug "compare op@\n";
       Eval.eval_list [e1; e2] man.eval flow |>
         Eval.bind (fun el flow ->
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

             man.eval (mk_py_call (mk_py_object (Addr.find_builtin "type") range) [e1] range) flow |>
               Eval.bind (fun cls1 flow ->
                   let cls1 = object_of_expr cls1 in
                   man.eval (mk_py_call (mk_py_object_attr cls1 op_fun range) [e1; e2] range) flow |>
                     Eval.bind (fun cmp flow ->
                         let not_implemented_type =
                           (* DONE? TODO: FIXME: ASK, issue with not_implemented *)
                           mk_py_call (mk_py_object (Addr.find_builtin "type") range) [mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range] range
                         in
                         let expr = (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [cmp; not_implemented_type] range) in
                         debug "Expr is %a@\n" pp_expr expr;
                         Eval.assume
                           expr
                           ~fthen:(fun true_flow ->
                             (* FIXME: subclass priority check is not implemented *)
                             begin
                               match op with
                               | O_eq | O_ne ->
                                  Eval.assume
                                    (mk_expr (E_binop(O_py_is, e1, e2)) range)
                                    ~fthen:(fun flow ->
                                      match op with
                                      | O_eq -> Eval.singleton (mk_py_true range) flow
                                      | O_ne -> Eval.singleton (mk_py_false range) flow
                                      | _ -> assert false)
                                    ~felse:(fun flow ->
                                      match op with
                                      | O_eq -> Eval.singleton (mk_py_false range) flow
                                      | O_ne -> Eval.singleton (mk_py_true range) flow
                                      | _ -> assert false)
                                    (* TODO *)
                                    (* ~fboth:(fun flow1 flow2 -> Eval.singleton (mk_py_top T_bool range) flow) *)
                                    man flow
                               | _ ->
                                  let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
                                  Eval.empty_singleton flow
                             end)
                           ~felse:(fun false_flow ->
                             Eval.singleton cmp flow)
                           man flow))) |> Option.return
    | _ -> None

  let exec _ _ _ _ = None
  let ask _ _ _ = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
