(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for arithmetic operators. *)


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
open Builtins

let name = "python.objects.data_model.arith_ops"
let debug fmt = Debug.debug ~channel:name fmt


module Domain = struct

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, e1, e2) when is_arith_op op ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

          let op_fun = binop_to_fun op in
          let rop_fun = binop_to_rev_fun op in

          if e1.etyp <> T_addr && e2.etyp <> T_addr then
            let f = mk_addr (Addr.find_type_function e1.etyp op_fun) range in
            let exp = mk_py_call f [e1; e2] range in
            re_eval_singleton (man.eval ctx) (Some exp, flow, [])
          else
            let cls1 = classof e1 and cls2 = classof e2 in
            let is_same_type e1 e2 =
              match ekind e1, ekind e2 with
              | E_addr {addr_kind = A_py_instance(cls1, _)}, E_addr {addr_kind = A_py_instance(cls2, _)} ->
                cls1 = cls2
              | E_addr _, _ | _, E_addr _ -> false
              | _ -> (compare_typ e1.etyp e2.etyp) = 0
            in

            let tmp = mktmp () in

            let add_cond = mk_builtin_call "hasattr" [e1; mk_string op_fun range] range in

            debug "Calling has_attribute";
            let has_add_flow = man.exec ctx (mk_assume add_cond range) flow in

            debug "Calling not has_attribute";
            let not_has_add_flow = man.exec ctx (mk_assume (mk_not add_cond range) range) flow in

            let post_add_flow =
              if man.flow.is_cur_bottom has_add_flow then
                man.flow.bottom
              else
                man.exec ctx
                  (mk_assign
                     (mk_var tmp range)
                     (mk_py_call (mk_py_addr_attr cls1 op_fun range) [e1; e2] range)
                     range
                  ) has_add_flow

            in

            let add_cases =
              if man.flow.is_cur_bottom post_add_flow then
                None
              else
                let flow = man.exec ctx
                    (mk_assume (mk_binop (mk_var tmp range) O_ne (mk_py_not_implemented range) range) range)
                    post_add_flow
                in
                re_eval_singleton (man.eval ctx) (Some (mk_var tmp range), flow, [mk_remove_var tmp range])

            in

            let add_error_case =
              if man.flow.is_cur_bottom post_add_flow && not (man.flow.is_cur_bottom has_add_flow) then
                let _ = debug "add error case" in
                oeval_singleton (None, post_add_flow, [mk_remove_var tmp range]) 
              else
                None
            in

            let pre_radd_flow =
              if is_same_type e1 e2 then
                man.flow.bottom
              else
                let add_notimplemeted_flow = man.exec ctx
                    (mk_assume (mk_binop (mk_var tmp range) O_eq (mk_py_not_implemented range) range) range)
                    post_add_flow
                in
                man.flow.join not_has_add_flow add_notimplemeted_flow
            in

            let has_radd_flow, not_has_radd_flow =
              if man.flow.is_cur_bottom pre_radd_flow then
                man.flow.bottom, man.flow.bottom
              else
                man.exec ctx (mk_assume (mk_builtin_call "hasattr" [e2; mk_string rop_fun range] range) range) pre_radd_flow,
                man.exec ctx (mk_assume (mk_not (mk_builtin_call "hasattr" [e2; mk_string rop_fun range] range) range) range) pre_radd_flow
            in
            let post_radd_flow =
              if man.flow.is_cur_bottom has_radd_flow then
                man.flow.bottom
              else
                man.exec ctx
                  (mk_assign
                     (mk_var tmp range)
                     (mk_py_call (mk_py_addr_attr cls2 rop_fun range) [e2; e1] range)
                     range
                  ) has_radd_flow
            in

            let radd_cases =
              if man.flow.is_cur_bottom post_radd_flow then
                None
              else
                let flow = man.exec ctx
                    (mk_assume (mk_binop (mk_var tmp range) O_ne (mk_py_not_implemented range) range) range)
                    post_radd_flow
                in
                re_eval_singleton (man.eval ctx) (Some (mk_var tmp range), flow, [mk_remove_var tmp range])

            in

            let type_error_flow =
              if is_same_type e1 e2 then (
                debug "same type";
                not_has_add_flow
              ) else (
                if man.flow.is_cur_bottom post_radd_flow then
                  man.flow.bottom
                else (
                  debug "check not implemented on@\ [%a]" man.flow.print post_radd_flow;
                  let add_notimplemeted_flow = man.exec ctx
                      (mk_assume (mk_binop (mk_var tmp range) O_eq (mk_py_not_implemented range) range) range)
                      post_radd_flow
                  in
                  debug "check not implemented on@\ [%a]" man.flow.print post_radd_flow;
                  man.flow.join not_has_radd_flow add_notimplemeted_flow
                )
              )
            in

            let type_error_cases =
              if man.flow.is_cur_bottom type_error_flow then
                None
              else
                let flow = man.exec ctx
                    (mk_builtin_raise "TypeError" range)
                    flow
                in
                oeval_singleton (None, flow, [mk_remove_var tmp range]) 

            in

            oeval_join add_cases add_error_case |> oeval_join radd_cases |> oeval_join type_error_cases

        )

    | E_unop(op, e) when is_arith_op op ->
      debug "Resolving unary operator %a" Framework.Pp.pp_operator op;
      man.eval ctx e flow |>
      eval_compose (fun e flow ->
          debug "Subexpression evaluated to %a(%a)" Framework.Pp.pp_expr e Framework.Pp.pp_typ e.etyp;
          let op_fun = unop_to_fun op in

          if e.etyp <> T_addr then
            let f = mk_addr (Addr.find_type_function e.etyp op_fun) range in
            let exp' = mk_py_call f [e] range in
            re_eval_singleton (man.eval ctx) (Some exp', flow, [])
          else
            let cls = classof e in
            let ok_cond = mk_builtin_call "hasattr" [e; mk_string op_fun range] range in
            let ok_flow = man.exec ctx (mk_assume ok_cond range) flow in
            let error_flow = man.exec ctx (mk_assume (mk_not ok_cond range) range) flow in

            let ok_case =
              if man.flow.is_cur_bottom ok_flow then
                None
              else
                let exp' = mk_py_call (mk_py_addr_attr cls op_fun range) [e] range in
                re_eval_singleton (man.eval ctx) (Some exp', ok_flow, [])
            in

            let error_case =
              if man.flow.is_cur_bottom error_flow then
                None
              else
                let flow = man.exec ctx
                    (mk_builtin_raise "TypeError" range)
                    flow
                in
                oeval_singleton (None, flow, [])
            in

            oeval_join ok_case error_case

        )

    | _ -> None

  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
