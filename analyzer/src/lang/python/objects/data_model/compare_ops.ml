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

          let cls1 = Addr.classof e1 in
          man.eval ctx (mk_py_call (mk_py_addr_attr cls1 op_fun range) [e1; e2] range) flow |>
          eval_compose (fun cmp flow ->
              match ekind cmp with
              | E_constant (C_py_not_implemented) ->
                (* FIXME: subclass priority check is not implemented *)
                begin
                  match op with
                  | O_eq | O_ne ->
                    Framework.Exceptions.panic "default O_eq/O_ne not yet implemented"
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
