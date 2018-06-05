(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python standard library. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Lattice
open Framework.Eval
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.libs.stdlib"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                               {2 Domain }                               *)
(*==========================================================================*)


module Domain =
struct


  (*==========================================================================*)
  (**                       {2 Transfer functions }                           *)
  (*==========================================================================*)


  let exec man ctx stmt flow = None

  let init _ ctx _ flow = ctx, flow

  let eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with

    (* Calls to iter built-in function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "iter")}},
        [obj], []
      )  ->
      (* Check that the class of obj has an attribute __iter__ *)
      man.eval ctx obj flow |>
      eval_compose (fun eobj flow ->
          let obj = object_of_expr eobj in
          let cls = Addr.class_of_object obj in
          Universal.Utils.assume_to_eval
            (Utils.mk_object_hasattr cls "__iter__" range)
            (fun true_flow ->
               (* Call iter and check that it returns an object with an attribute __next__ *)
               man.eval ctx (mk_py_call (mk_py_object_attr cls "__iter__" range) [eobj] range) true_flow |>
               eval_compose (fun iter flow ->
                   Universal.Utils.assume_to_eval
                     (Utils.mk_hasattr iter "__next__" range)
                     (fun true_flow -> oeval_singleton (Some iter, true_flow, []))
                     (fun false_flow -> oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow, []))
                     man ctx flow ()
                 )
            )
            (fun false_flow -> oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow, []))
            man ctx flow ()
        )

    (* Calls to len built-in function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "len")}},
        [obj], []
      )  ->
      (* Check that the class of obj has an attribute __len__ *)
      man.eval ctx obj flow |>
      eval_compose (fun eobj flow ->
          let obj = object_of_expr eobj in
          let cls = Addr.class_of_object obj in
          Universal.Utils.assume_to_eval
            (Utils.mk_object_hasattr cls "__len__" range)
            (fun true_flow ->
               (* Call __len__ and check that it returns an integer *)
               man.eval ctx (mk_py_call (mk_py_object_attr cls "__len__" range) [eobj] range) true_flow |>
               eval_compose (fun len flow ->
                   match etyp len with
                   | T_int -> oeval_singleton (Some len, flow, [])
                   | _ -> oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) true_flow, [])
                 )
            )
            (fun false_flow -> oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow, []))
            man ctx flow ()
        )

    (* Calls to built-in function next *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "next")}},
        [obj], []
      )  ->
      (* Check that the class of obj has an attribute __next__ *)
      man.eval ctx obj flow |>
      eval_compose (fun eobj flow ->
          let obj = object_of_expr eobj in
          let cls = Addr.class_of_object obj in
          Universal.Utils.assume_to_eval
            (Utils.mk_object_hasattr cls "__next__" range)
            (fun true_flow ->
               (Some (mk_py_call (mk_py_object_attr cls "__next__" range) [eobj] range), true_flow, []) |>
               re_eval_singleton (man.eval ctx)
            )
            (fun false_flow -> oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow, []))
            man ctx flow ()
        )

    (* Calls to isinstance built-in function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "isinstance")}},
        [obj; cls], []
      )  ->
      eval_list [obj; cls] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let eobj, ecls = match el with [obj; cls] -> obj, cls | _ -> assert false in
          let obj = object_of_expr eobj in
          let cls = object_of_expr cls in
          (* cases of cls: class or tuple of classes *)
          if Addr.isclass cls then
            let cls0 = Addr.class_of_object obj in
            if Addr.issubclass cls0 cls then
              oeval_singleton (Some (mk_true range), flow, [])
            else
              oeval_singleton (Some (mk_false range), flow, [])
          else
          if Addr.isinstance cls (Addr.find_builtin "tuple") then
            Framework.Exceptions.panic_at range "isinstance: tuple of classes not supported"
          else
            oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow, [])
        )

    (* Calls to issubclass built-in function *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "issubclass")}},
        [cls1; cls2], []
      )  ->
      eval_list [cls1; cls2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let ecls1, ecls2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          let cls1 = object_of_expr ecls1 in
          let cls2 = object_of_expr ecls2 in

          if Addr.isclass cls1 && Addr.isclass cls2 then
            if Addr.issubclass cls1 cls2 then
              oeval_singleton (Some (mk_true range), flow, [])
            else
              oeval_singleton (Some (mk_false range), flow, [])
          else
          if Addr.isinstance cls2 (Addr.find_builtin "tuple") then
            Framework.Exceptions.panic_at range "issubclass: tuple of classes not supported"
          else
            oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow, [])
        )

    | _ ->
      None

  let ask _ _ _ _ = None

end




(*==========================================================================*)
(**                             {2 Setup }                                  *)
(*==========================================================================*)


let setup () =
  Stateless.register_domain name (module Domain)
