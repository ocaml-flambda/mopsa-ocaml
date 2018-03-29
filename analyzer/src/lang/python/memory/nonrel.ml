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
open Framework.Domains.Global
open Framework.Flow
open Framework.Manager
open Framework.Query
open Framework.Ast
open Universal.Ast
open Ast
open Value

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

  let exec stmt (man: ('a, t) manager) (ctx: Framework.Context.context) (flow: 'a flow) : 'a flow option =
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
    | S_assign({ekind = E_var var} as evar, e, ((STRONG | EXPAND) as kind)) ->
      Eval.compose_exec
        e
        (fun e flow ->
           match ekind e with
           | E_addr(addr) ->
             let v = Value.addr (Value.AddrLattice.singleton addr) in
             map_domain_cur (Nonrel.add var v) man flow |>
             Exec.return
           | _ ->
             Nonrel.exec {stmt with skind = S_assign (evar, e,kind)} man ctx flow
        )
        (fun flow -> Exec.return flow)
        man ctx flow

    | S_assign({ekind = E_var var}, e, WEAK) ->
      assert false

    (* Modify variables already pointing to a1 in order to point to a2. *)
    | S_rebase_addr(a1, a2) ->
      map_domain_cur (
        Nonrel.map (fun v -> Value.rebase_addr a1 a2 v)
      ) man flow |>
      Exec.return

    (* Other cases are handled by {!Nonrel}. *)
    | _ ->
      Nonrel.exec stmt man ctx flow

  let ask query man ctx flow  =
    Nonrel.ask query man ctx flow

  let eval exp man ctx flow =
    let range = erange exp in
    match ekind exp with
    | E_var v when Builtins.is_builtin v.vname ->
      debug "builtin";
      Eval.singleton (Some (mk_addr (Builtins.from_expr exp) range), flow, [])

    (* Refine the type of a variable using its current abstract value *)
    | E_var v  ->
      let nonrel = get_domain_cur man flow in
      let value = Nonrel.find v nonrel in
      Value.fold_type (fun acc (typ, value') ->
          let nonrel' = Nonrel.add v value' nonrel in
          let flow' = set_domain_cur nonrel' man flow in
          match typ with
          (* Raise an exception when the variable maybe undefined *)
          | T_py_undefined ->
            let stmt = Builtins.mk_builtin_raise "UnboundLocalError" (tag_range range "undef") in
            let flow = man.exec stmt ctx flow' in
            Eval.singleton (None, flow, []) |>
            Eval.join acc

          (* Partition w.r.t. to all current addresses *)
          | T_addr ->
            if Value.AddrLattice.is_top value'.addr then
              panic "top address found"
            else
              Value.AddrLattice.fold (fun addr acc ->
                  (* TODO: refine cur by pointing v to a singleton address addr *)
                  let exp' = {exp with ekind = E_addr addr; etyp = typ} in
                  Eval.singleton (Some exp', flow', []) |>
                  Eval.join acc
                ) value'.addr acc
          (* Otherwise, we just give type [typ] to the expression *)
          | _ ->
            let exp' = {exp with etyp = typ} in
            Eval.singleton (Some exp', flow', []) |>
            Eval.join acc

      ) None value


    | _ ->
      Nonrel.eval exp man ctx flow


end

let setup () =
  Framework.Domains.Global.register_domain name (module Domain);
  ()
