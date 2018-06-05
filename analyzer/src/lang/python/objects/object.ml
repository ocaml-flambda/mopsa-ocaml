(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Definition of 'object' class methods. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.objects.object"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let init _ ctx _ flow = ctx, flow

  let exec man ctx stmt flow = None

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    (* Calls to object.__new__ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__new__")}, _)}, args, []) ->
      debug "call to object.__new__";
      eval_list args (man.eval ctx) flow |>
      eval_compose
        (fun args flow ->
           match args with
           | cls :: tl ->
             let cls = object_of_expr cls in
             eval_alloc_instance man ctx cls None range flow |>
             oeval_compose (fun obj flow ->
                 oeval_singleton (Some (mk_py_object obj range), flow, [])
               )

           | _ ->
             debug "Error in creating a new instance";
             let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
             oeval_singleton (None, flow, [])
        )

    (* Calls to object.__init__ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__init__")}, _)}, args, []) ->
      oeval_singleton (Some (mk_py_none range), flow, [])


    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
