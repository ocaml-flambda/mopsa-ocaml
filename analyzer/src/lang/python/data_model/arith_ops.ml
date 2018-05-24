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
open Operators

let name = "python.data_model.arith_ops"
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
            let cls1 = Addr.classof e1 in
            let f = mk_py_addr_attr cls1  op_fun range in
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

            Universal.Utils.assume_to_eval
              (Utils.mk_builtin_call "hasattr" [mk_addr cls1 range; mk_string op_fun range] range)
              (fun true_flow ->
                 man.eval ctx (mk_py_call (mk_py_addr_attr cls1 op_fun range) [e1; e2] range) true_flow |>
                 eval_compose (fun r flow ->
                     match ekind r with
                     | E_constant (C_py_not_implemented) ->
                       if is_same_type e1 e2 then
                         let flow = man.exec ctx
                             (Utils.mk_builtin_raise "TypeError" range)
                             flow
                         in
                         oeval_singleton (None, flow, [])
                       else
                         Universal.Utils.assume_to_eval
                           (Utils.mk_builtin_call "hasattr" [mk_addr cls2 range; mk_string rop_fun range] range)
                           (fun true_flow ->
                              man.eval ctx (mk_py_call (mk_py_addr_attr cls2 rop_fun range) [e2; e1] range) true_flow |>
                              eval_compose (fun r flow ->
                                  match ekind r with
                                  | E_constant (C_py_not_implemented) ->
                                    let flow = man.exec ctx
                                        (Utils.mk_builtin_raise "TypeError" range)
                                        flow
                                    in
                                    oeval_singleton (None, flow, [])
                                  | _ ->
                                    oeval_singleton (Some r, flow, [])
                                )
                           )
                           (fun false_flow ->
                              let flow = man.exec ctx
                                  (Utils.mk_builtin_raise "TypeError" range)
                                  false_flow
                              in
                              oeval_singleton (None, flow, [])

                           )
                           man ctx flow ()
                     | _ ->
                       oeval_singleton (Some r, flow, [])
                   )
              )
              (fun false_flow ->
                 if is_same_type e1 e2 then
                   let flow = man.exec ctx
                       (Utils.mk_builtin_raise "TypeError" range)
                       flow
                   in
                   oeval_singleton (None, flow, [])
                 else
                   Universal.Utils.assume_to_eval
                     (Utils.mk_builtin_call "hasattr" [mk_addr cls2 range; mk_string rop_fun range] range)
                     (fun true_flow ->
                        man.eval ctx (mk_py_call (mk_py_addr_attr cls2 rop_fun range) [e2; e1] range) true_flow |>
                        eval_compose (fun r flow ->
                            match ekind r with
                            | E_constant (C_py_not_implemented) ->
                              let flow = man.exec ctx
                                  (Utils.mk_builtin_raise "TypeError" range)
                                  flow
                              in
                              oeval_singleton (None, flow, [])
                            | _ ->
                              oeval_singleton (Some r, flow, [])
                          )
                     )
                     (fun false_flow ->
                        let flow = man.exec ctx
                            (Utils.mk_builtin_raise "TypeError" range)
                            flow
                        in
                        oeval_singleton (None, flow, [])

                     )
                     man ctx flow ()
              )
              man ctx flow ()
        )
        
    | E_unop(op, e) when is_arith_op op ->
      debug "Resolving unary operator %a" Framework.Pp.pp_operator op;
      man.eval ctx e flow |>
      eval_compose (fun e flow ->
          debug "Subexpression evaluated to %a(%a)" Framework.Pp.pp_expr e Framework.Pp.pp_typ e.etyp;
          let op_fun = unop_to_fun op in

          if e.etyp <> T_addr then
            let cls = Addr.classof e in
            let f = mk_py_addr_attr cls op_fun range in
            let exp' = mk_py_call f [e] range in
            re_eval_singleton (man.eval ctx) (Some exp', flow, [])
          else
            let cls = classof e in
            Universal.Utils.assume_to_eval
              (Utils.mk_builtin_call "hasattr" [mk_addr cls range; mk_string op_fun range] range)
              (fun true_flow ->
                 man.eval ctx (mk_py_call (mk_py_addr_attr cls op_fun range) [e] range) true_flow |>
                 return
              )
              (fun false_flow ->
                 let flow = man.exec ctx
                     (Utils.mk_builtin_raise "TypeError" range)
                     false_flow
                 in
                 oeval_singleton (None, flow, [])
              )
              man ctx flow ()
        )

    | _ -> None

  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
