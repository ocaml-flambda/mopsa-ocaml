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

(** Utility functions for operators *)

let is_arith_op = function
  | O_plus T_any
  | O_minus T_any
  | O_mult T_any
  | O_py_mat_mult
  | O_div T_any
  | O_py_floor_div
  | O_mod T_any
  | O_pow
  | O_bit_lshift
  | O_bit_rshift
  | O_bit_and
  | O_bit_xor
  | O_bit_or ->
    true

  | _ -> false


let all_arithmetic_ops = [
  "add"; "sub"; "mul"; "matmul"; "truediv"; "floordiv"; "mod"; "divmod"; "pow"; "lshift"; "rshift"; "and"; "xor"; "or"
]

let string_to_binop = function
  | "add" -> O_plus T_any
  | "sub" -> O_minus T_any
  | "mul" -> O_mult T_any
  | "matmul" -> O_py_mat_mult
  | "truediv" -> O_div T_any
  | "floordiv" -> O_py_floor_div
  | "mod" -> O_mod T_any
  | "pow" -> O_pow
  | "lshift" -> O_bit_lshift
  | "rshift" -> O_bit_rshift
  | "and" -> O_bit_and
  | "xor" -> O_bit_xor
  | "or" -> O_bit_or
  | _ -> assert false

let binop_to_string = function
  | O_plus T_any -> "add"
  | O_minus T_any -> "sub"
  | O_mult T_any -> "mul"
  | O_py_mat_mult -> "matmul"
  | O_div T_any -> "truediv"
  | O_py_floor_div -> "floordiv"
  | O_mod T_any -> "mod"
  | O_pow -> "pow"
  | O_bit_lshift -> "lshift"
  | O_bit_rshift -> "rshift"
  | O_bit_and -> "and"
  | O_bit_xor -> "xor"
  | O_bit_or -> "or"
  | _ -> assert false

let string_to_unop = function
  | "not" -> O_log_not
  | "neg" -> O_minus T_any
  | "pos" -> O_plus T_any
  | "invert" -> O_bit_invert
  | _ -> assert false


let unop_to_string = function
  | O_log_not -> "not"
  | O_plus T_any -> "pos"
  | O_minus T_any -> "neg"
  | O_bit_invert -> "invert"
  | _ -> assert false


let all_unops = [
 "neg"; "pos"; "abs"; "invert"
]

let all_convert = [
  "complex"; "int"; "float"; "round"
]

let op_fun op = "__" ^ op ^ "__"

let op_fun_inv f =
  if Str.string_match (Str.regexp "__\\(.*\\)__") f 0 then
    Str.matched_group 1 f
  else
    raise Not_found

let rop_fun op = op_fun ("r" ^ op)

let op_funs = List.map op_fun
let rop_funs = List.map rop_fun

let all_arithmetic_functions = op_funs all_arithmetic_ops
let all_unops_functions = op_funs all_unops


module Domain = struct

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, e1, e2) when is_arith_op op ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

          let op_fun = binop_to_string op |> op_fun in
          let rop_fun = binop_to_string op |> rop_fun in

          if e1.etyp <> T_addr && e2.etyp <> T_addr then
            let exp = mk_py_call (mk_py_attr e1 op_fun range) [e2] range in
            re_eval_singleton (man.eval ctx) (Some exp, flow, [])
          else
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
                     (mk_py_call (mk_py_attr e1 op_fun range) [e2] range)
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
                     (mk_py_call (mk_py_attr e2 rop_fun range) [e1] range)
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
          debug "Subexpression evaluated to %a" Framework.Pp.pp_expr e;
          let op_fun =
            match op with
            | O_plus T_any -> "__pos__"
            | O_minus T_any -> "__neg__"
            | O_bit_invert -> "__inverert__"
            | _ -> assert false
          in

          if e.etyp <> T_addr then
            let exp' = mk_py_call (mk_py_attr e op_fun range) [] range in
            re_eval_singleton (man.eval ctx) (Some exp', flow, [])
          else
            let ok_cond = mk_builtin_call "hasattr" [e; mk_string op_fun range] range in
            let ok_flow = man.exec ctx (mk_assume ok_cond range) flow in
            let error_flow = man.exec ctx (mk_assume (mk_not ok_cond range) range) flow in

            let ok_case =
              if man.flow.is_cur_bottom ok_flow then
                None
              else
                let exp' = mk_py_call (mk_py_attr e op_fun range) [] range in
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
