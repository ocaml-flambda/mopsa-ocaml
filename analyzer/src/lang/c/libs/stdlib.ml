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

(** Kinds of heap allocated addresses. *)
type Universal.Ast.addr_kind +=
  | A_c_static_malloc of Z.t (** static size *)
  | A_c_dynamic_malloc (** dynamic size *)


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
             let exp' = Universal.Ast.mk_alloc_addr (A_c_static_malloc z) exp.erange exp.erange in
             Framework.Eval.re_eval_singleton (man.eval ctx) (Some exp', flow, [])

           (* Allocation with a dynamic size *)
           | None ->
             let exp' = Universal.Ast.mk_alloc_addr A_c_dynamic_malloc exp.erange exp.erange in
             Framework.Eval.re_eval_singleton (man.eval ctx) (Some exp', flow, [])

        )

    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain);
  Universal.Ast.register_addr_kind_compare (fun next ak1 ak2 ->
      match ak1, ak2 with
      | A_c_static_malloc s1, A_c_static_malloc s2 -> Z.compare s1 s2
      | _ -> next ak1 ak2
    );
  Universal.Pp.register_pp_addr_kind (fun next fmt ak ->
      match ak with
      | A_c_static_malloc s -> Format.fprintf fmt "static malloc(%a)" Z.pp_print s
      | A_c_dynamic_malloc -> Format.fprintf fmt "dynamic malloc"
      | _ -> next fmt ak
    )
