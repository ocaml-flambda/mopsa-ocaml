(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of standard C library *)

open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Lattice
open Framework.Domains.Stateless
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.libs.stdlib"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let is_builtin_function = function
    | "malloc" -> true
    | _ -> false

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow

  let exec man ctx stmt flow = None

  let eval man ctx exp flow =
    debug "at least it came here : %a" Framework.Pp.pp_expr exp;
    match ekind exp with
    | E_c_function(f) when is_builtin_function f.c_func_var.vname ->
      debug "builtin function";
      let exp' = mk_expr (E_c_builtin_function f.c_func_var.vname) ~etyp:T_c_builtin_fn exp.erange in
      oeval_singleton (Some exp', flow, [])

    | E_c_call({ekind = E_c_builtin_function "malloc"}, [size]) ->
      man.eval ctx size flow |>
      Framework.Eval.eval_compose
        (fun size flow ->

           (* try to convert [size] into a constant *)
           match Universal.Utils.expr_to_z size with

           (* Allocation with a static size *)
           | Some z ->
             let exp' = Universal.Ast.mk_alloc_addr (Memory.Cell.A_c_static_malloc z) exp.erange exp.erange in
             Framework.Eval.re_eval_singleton (man.eval ctx) (Some exp', flow, [])

           (* Allocation with a dynamic size *)
           | None ->
             let exp' = Universal.Ast.mk_alloc_addr Memory.Cell.A_c_dynamic_malloc exp.erange exp.erange in
             Framework.Eval.re_eval_singleton (man.eval ctx) (Some exp', flow, [])

        )

    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain)
