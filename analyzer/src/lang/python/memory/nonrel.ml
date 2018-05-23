(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Non-relational abstract environment.

   {!Universal.Nonrel.Domain} is used to instantiate the domain on
   Python-specific {!Value} abstraction.
*)

open Framework.Domains
open Framework.Domains.Reduce.Domain
open Framework.Flow
open Framework.Manager
open Framework.Query
open Framework.Eval
open Framework.Exec
open Framework.Ast
open Framework.Utils
open Universal.Ast
open Ast
open Value
open Addr

let name = "python.memory.nonrel"

module Domain =
struct

  (*==========================================================================*)
  (**                         {2 Lattice structure}                           *)
  (*==========================================================================*)

  module Nonrel = Universal.Nonrel.Domain.Make(Value)

  include Nonrel

  let print fmt abs =
    Format.fprintf fmt "non-rel:@,@[<v2>  %a@]@\n"
      Nonrel.print abs

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let exec (man: ('a, t) manager) (ctx: Framework.Context.context) stmt (flow: 'a flow) : 'a rflow option =
    (* Before executing the statement, we need to remove the type of variables.
         This is necessary to keep the same key for all possible (typed) values of a variable in the non-relational map.
    *)
    let stmt = Framework.Visitor.map_stmt
        (function {ekind = E_var v} as e -> {e with ekind = E_var {v with vtyp = T_any}} | x -> x)
        (function x -> x)
        stmt
    in
    match skind stmt with
    (* For assignments, we handle the case of addresses, otherwise we give it
       to {!Nonrel}. *)
    | S_assign({ekind = E_var var} as evar, e, mode) ->
      man.eval ctx e flow |>
      eval_to_orexec
        (fun e flow ->
           match ekind e with
           | E_addr(addr) ->
             let v = Value.addr (Value.A.singleton addr) in
             let flow' = match mode with
               | STRONG | EXPAND ->
                 map_domain_cur (Nonrel.add var v) man flow
               | WEAK ->
                 let old = get_domain_cur man flow |> find var in
                 let v' = Value.join old v in
                 map_domain_cur (Nonrel.add var v') man flow
             in
             return_flow flow'
           | _ ->
             Nonrel.exec man ctx {stmt with skind = S_assign (evar, e, mode)} flow
        )
        (man.exec ctx) man.flow

    (* Modify variables already pointing to a1 in order to point to a2. *)
    | S_rebase_addr(a1, a2, mode) ->
      Framework.Exceptions.panic "python heap operations not supported"
      (* map_domain_cur (
       *   Nonrel.map (fun v -> Value.rebase_addr a1 a2 v)
       * ) man flow |>
       * return *)

    (* Other cases are handled by {!Nonrel}. *)
    | _ ->
      Nonrel.exec man ctx stmt flow

  let ask : type r. ('a, t) manager -> Framework.Context.context -> r Framework.Query.query -> 'a flow -> r option =
    fun man ctx query flow ->
      match query with
      | Universal.Numeric.Query.QIntInterval (e) ->
        begin
          let e' = Framework.Visitor.map_expr
              (function {ekind = E_var v} as e -> {e with ekind = E_var {v with vtyp = T_any}} | x -> x)
              (function x -> x)
              e
          in
          let cur = get_domain_cur man flow in
          let v = Nonrel.eval_value cur e' in
          match type_of v with
          | [T_int] -> Some v.int
          | _ -> None
        end

      | _ -> Nonrel.ask man ctx query flow

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_var v when is_builtin v.vname ->
      debug "builtin";
      oeval_singleton (Some (mk_addr (Addr.find_builtin v.vname) range, []), flow, [])

    (* Return an empty expression when cur is bottom *)
    | E_var v when man.flow.is_cur_bottom flow -> oeval_singleton (None, flow, [])

    (* Refine the type of a variable using its current abstract value *)
    | E_var v ->
      debug "var %a" Framework.Pp.pp_var v;
      let nonrel = get_domain_cur man flow in
      let value = Nonrel.find v nonrel in
      Value.fold_type (fun acc (typ, value') ->
          let nonrel' = Nonrel.add v value' nonrel in
          let flow' = set_domain_cur nonrel' man flow in
          match typ with
          (* Raise an exception when the variable maybe undefined *)
          | T_py_undefined ->
            let stmt = Utils.mk_builtin_raise "UnboundLocalError" (tag_range range "undef") in
            let flow = man.exec ctx stmt flow' in
            oeval_singleton (None, flow, []) |>
            oeval_join acc

          (* Partition w.r.t. to all current addresses *)
          | T_addr ->
            if Value.A.is_top value'.addr then
              Framework.Exceptions.panic "top address found"
            else
              Value.A.fold (fun addr acc ->
                  (* TODO: refine cur by pointing v to a singleton address addr *)
                  let exp' = {exp with ekind = E_addr addr; etyp = typ} in
                  oeval_singleton (Some (exp', []), flow', []) |>
                  oeval_join acc
                ) value'.addr acc
          (* Otherwise, we just give type [typ] to the expression *)
          | _ ->
            let exp' = {exp with etyp = typ} in
            oeval_singleton (Some (exp', []), flow', []) |>
            oeval_join acc

      ) None value

    (* TODO: this should be moved to data model of operators *)
    | E_unop(op, e) ->
      man.eval ctx e flow |>
      eval_compose
        (fun e flow ->
           let exp' = {exp with ekind = E_unop(op, e)} in
           oeval_singleton (Some (exp', []), flow, [])
        )

    (* TODO: this should be moved to data model of operators *)
    | E_binop(op, e1, e2) ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose
        (fun el flow ->
           let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
           let exp' = {exp with ekind = E_binop(op, e1, e2)} in
           oeval_singleton (Some (exp', []), flow, [])
        )

    | _ ->
      Nonrel.eval man ctx exp flow


end

let setup () =
  register_domain name (module Domain);
  ()
