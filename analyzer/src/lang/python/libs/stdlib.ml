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
      eval_compose (fun obj flow ->
          let cls = classof obj in
          Universal.Utils.assume_to_eval
            (Utils.mk_addr_hasattr cls "__iter__" range)
            (fun true_flow ->
               (* Call iter and check that it returns an object with an attribute __next__ *)
               man.eval ctx (mk_py_call (mk_py_addr_attr cls "__iter__" range) [obj] range) true_flow |>
               eval_compose (fun iter flow ->
                   match ekind iter with
                   | E_addr iter ->
                     Universal.Utils.assume_to_eval
                       (Utils.mk_addr_hasattr iter "__next__" range)
                       (fun true_flow -> oeval_singleton (Some (mk_addr iter range), true_flow, []))
                       (fun false_flow -> oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow, []))
                       man ctx flow ()

                   | _ ->
                     oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow, [])
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
      eval_compose (fun obj flow ->
          let cls = classof obj in
          Universal.Utils.assume_to_eval
            (Utils.mk_addr_hasattr cls "__len__" range)
            (fun true_flow ->
               (* Call __len__ and check that it returns an integer *)
               man.eval ctx (mk_py_call (mk_py_addr_attr cls "__len__" range) [obj] range) true_flow |>
               eval_compose (fun len flow ->
                   match etyp len with
                   | T_int -> oeval_singleton (Some len, flow, [])
                   | _ -> oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) true_flow, [])
                 )
            )
            (fun false_flow -> oeval_singleton (None, man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow, []))
            man ctx flow ()
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
