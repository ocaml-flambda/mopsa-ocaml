(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for subscript access. *)

open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Flow
open Universal.Ast
open Framework.Domains.Stateless
open Framework.Exec
open Universal.Ast
open Ast
open Addr

let name = "python.data_model.subscript"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_index_subscript(obj, index) ->
      eval_list [obj; index] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let eobj, index = match el with [obj; index] -> obj, index | _ -> assert false in
          let obj = object_of_expr eobj in
          let cls = Addr.class_of_object obj in

          Universal.Utils.assume_to_eval
            (Utils.mk_object_hasattr cls "__getitem__" range)
            (fun true_flow ->
              let exp' = mk_py_call (mk_py_object_attr cls "__getitem__" range) [eobj; index] range in
              re_eval_singleton (man.eval ctx) (Some exp', true_flow, [])
            )
            (fun false_flow ->
               let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow in
               oeval_singleton (None, flow, [])
            )
            man ctx flow ()
        )

    | E_py_slice_subscript(obj, start, stop, step) ->
      eval_list [obj; start; stop; step] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let eobj, start, stop, step = match el with [obj; start; stop; step] -> obj, start, stop, step | _ -> assert false in
          let obj = object_of_expr eobj in
          let cls = Addr.class_of_object obj in

          Universal.Utils.assume_to_eval
            (Utils.mk_object_hasattr cls "__getitem__" range)
            (fun true_flow ->
               man.eval ctx (Utils.mk_builtin_call "slice" [start; stop; step] range) true_flow |>
               eval_compose (fun slice flow ->
                   let exp' = mk_py_call (mk_py_object_attr cls "__getitem__" range) [eobj; slice] range in
                   re_eval_singleton (man.eval ctx) (Some exp', flow, [])
                 )
            )
            (fun false_flow ->
               let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow in
               oeval_singleton (None, flow, [])
            )
            man ctx flow ()
        )


    | _ -> None

  let exec man ctx stmt flow =
    let range = stmt.srange in
    match skind stmt with
    | S_assign({ekind = E_py_index_subscript(obj, index)}, exp, mode) ->
      eval_list [exp; obj; index] (man.eval ctx) flow |>
      eval_to_exec
        (fun el flow ->
           let exp, eobj, index = match el with [exp; obj; index] -> exp, obj, index | _ -> assert false in
           let obj = object_of_expr eobj in
           let cls = Addr.class_of_object obj in

           Universal.Utils.assume_to_exec
             (Utils.mk_object_hasattr cls "__setitem__" range)
             (fun true_flow ->
                let exp' = mk_py_call (mk_py_object_attr cls "__setitem__" range) [eobj; index; exp] range in
                man.exec ctx {stmt with skind = S_expression(exp')} true_flow
             )
             (fun false_flow ->
                man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow
             )
             man ctx flow ()
             
        ) (man.exec ctx) man.flow |>
      return

    | S_assign({ekind = E_py_slice_subscript (obj, start, stop, step)}, exp, mode) ->
      eval_list [exp; obj; start; stop; step] (man.eval ctx) flow |>
      eval_to_exec (fun el flow ->
          let exp, eobj, start, stop, step = match el with [exp; obj; start; stop; step] -> exp, obj, start, stop, step | _ -> assert false in
           let obj = object_of_expr eobj in
           let cls = Addr.class_of_object obj in

          Universal.Utils.assume_to_exec
            (Utils.mk_object_hasattr cls "__setitem__" range)
            (fun true_flow ->
               man.eval ctx (Utils.mk_builtin_call "slice" [start; stop; step] range) true_flow |>
               eval_to_exec (fun slice flow ->
                   let exp' = mk_py_call (mk_py_object_attr cls "__setitem__" range) [eobj; slice; exp] range in
                   man.exec ctx {stmt with skind = S_expression(exp')} flow
                 ) (man.exec ctx) man.flow
            )
            (fun false_flow ->
               man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow
            )
            man ctx flow ()
        ) (man.exec ctx) man.flow |>
      return


    | _ -> None

  let init man ctx prog flow = ctx, flow
  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain)
