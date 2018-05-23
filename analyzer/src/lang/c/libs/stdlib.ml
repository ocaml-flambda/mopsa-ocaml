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

let range_exp_of_c_type t range =
  let rmin, rmax = rangeof t in
  (mk_expr ~etyp:t (E_constant (C_int_interval(rmin,rmax))) range)

module Domain =
struct

  let is_builtin_function = function
    | "malloc" -> true
    | "fscanf" -> true
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

    | E_c_call({ekind = E_c_builtin_function "fscanf"}, l) ->
      let () = debug "fscanf called" in
      let range_exp = erange exp in
      let input_variables = match l with
        | p::q::r -> r
        | _ -> debug "fscanf called on less than two arguments" ; assert false
      in
      let stmts = List.map (
          fun e ->
            let range = erange e in
            let t = etyp e in
            if is_c_pointer_type t then
              let t' = under_pointer_type t in
              mk_assign (mk_expr ~etyp:t' (E_c_deref e) (tag_range range "lv"))
                (range_exp_of_c_type t' (tag_range range "rv"))
                (tag_range range "assign")
            else
              (debug "fscanf called on arguments that is not a pointer" ; assert false)
        ) input_variables in
      let s = mk_block stmts (tag_range range_exp "block") in
      let flow' = man.exec ctx s flow in
      re_eval_singleton (man.eval ctx)
        (Some (range_exp_of_c_type (etyp exp) (tag_range range_exp "return")), flow', [])
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
  Universal.Pp.register_pp_addr (fun next fmt addr ->
      match addr.addr_kind, Universal.Heap.Recency.is_weak addr with
      | A_c_static_malloc s, false-> Format.fprintf fmt "@@{static(%a), %a}" Z.pp_print s Framework.Pp.pp_range addr.addr_range
      | A_c_static_malloc s, true-> Format.fprintf fmt "@@{static(%a), %a, weak}" Z.pp_print s Framework.Pp.pp_range addr.addr_range
      | A_c_dynamic_malloc, false -> Format.fprintf fmt "@@{dynamic, %a}" Framework.Pp.pp_range addr.addr_range
      | A_c_dynamic_malloc, true -> Format.fprintf fmt "@@{dynamic, %a, weak}" Framework.Pp.pp_range addr.addr_range
      | _ -> next fmt addr
    )
