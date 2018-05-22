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

let name = "python.objects.data_model.compare_ops"
let debug fmt = Debug.debug ~channel:name fmt


module Domain = struct

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, ({etyp = T_any} as e1), ({etyp = T_any} as e2)) when is_comp_op op ->
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

          let cls1 = Addr.classof e1 in
          (* and cls2 = Addr.classof e2 in *)
          (* FIXME: subclass priority check is not implemented *)

          let tmp = mktmp () in
          let post_op_flow = man.exec ctx
              (mk_assign
                 (mk_var tmp range)
                 (mk_py_call (mk_py_addr_attr cls1 op_fun range) [e1; e2] range)
                 range
              ) flow
          in

          let not_ni_flow = man.exec ctx
              (mk_assume (mk_binop (mk_var tmp range) O_ne (mk_py_not_implemented range) range) range)
              post_op_flow
          in

          let not_ni_case =
            if man.flow.is_cur_bottom not_ni_flow then
              None
            else
              re_eval_singleton (man.eval ctx) (Some (mk_var tmp range), not_ni_flow, [mk_remove_var tmp range])
          in

          let ni_flow = man.exec ctx
              (mk_assume (mk_binop (mk_var tmp range) O_eq (mk_py_not_implemented range) range) range)
              post_op_flow
          in

          let ni_case =
            if man.flow.is_cur_bottom ni_flow then
                  None
            else
              match op with
              | O_eq | O_ne ->
                Framework.Exceptions.panic "default O_eq/O_ne not yet implemented"
              | _ ->
                let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) ni_flow in
                oeval_singleton (None, flow, [])

          in

          oeval_join not_ni_case ni_case

        )

    | _ -> None


  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
