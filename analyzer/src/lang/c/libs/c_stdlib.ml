(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of standard C library *)

open Framework.Essentials
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

module Domain : Framework.Domains.Stateless.S =
struct

  let is_builtin_function = function
    | "malloc" -> true
    | "fscanf" -> true
    | _ -> false

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_libs_stdlib : unit domain
  let id = D_c_libs_stdlib
  let name = "c.libs.stdlib"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_libs_stdlib -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = [Zone.Z_c]; import = []}
  let eval_interface = {export = []; import = []}

  (** Initialization *)
  (** ============== *)

  let init prog man (flow: 'a flow) =
    None

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let exec zone stmt man flow = None

  let eval zone exp man flow =
    match ekind exp with
    | E_c_function(f) when is_builtin_function f.c_func_var.vname ->
      debug "builtin function";
      let exp' = mk_expr (E_c_builtin_function f.c_func_var.vname) ~etyp:T_c_builtin_fn exp.erange in
      (Eval.singleton exp' flow) |> Option.return

    | E_c_call({ekind = E_c_builtin_function "malloc"}, [size]) ->
      begin
        man.eval size flow |> Eval.bind @@ fun size flow ->

        (* try to convert [size] into a constant *)
        match Universal.Utils.expr_to_z size with

        (* Allocation with a static size *)
        | Some z ->
          let exp' = Universal.Ast.mk_alloc_addr (A_c_static_malloc z) exp.erange in
          man.eval exp' flow

        (* Allocation with a dynamic size *)
        | None ->
          let exp' = Universal.Ast.mk_alloc_addr A_c_dynamic_malloc exp.erange in
          man.eval exp' flow
      end |> Option.return

    | E_c_call({ekind = E_c_builtin_function "fscanf"}, l) ->
      begin
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
        let flow' = man.exec s flow in
        man.eval (range_exp_of_c_type (etyp exp) (tag_range range_exp "return")) flow'
      end |> Option.return
    | _ -> None

  let ask _ _ _  = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain);
  Universal.Ast.register_addr (
    {compare = (fun next ak1 ak2 ->
        match ak1.addr_kind, ak2.addr_kind with
          | A_c_static_malloc s1, A_c_static_malloc s2 -> Z.compare s1 s2
          | _ -> next ak1 ak2);
     print = fun next fmt addr ->
       (* Fixme improve printing to account for weak or strong recency abstraction *)
       match addr.addr_kind with
       | A_c_static_malloc s-> Format.fprintf fmt "@@{static(%a), %a}" Z.pp_print s Format.pp_print_int addr.addr_uid
       | A_c_dynamic_malloc -> Format.fprintf fmt "@@{dynamic, %a}" Format.pp_print_int addr.addr_uid
       | _ -> next fmt addr
    })
