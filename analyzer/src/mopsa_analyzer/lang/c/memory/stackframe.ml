(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** This domain ensures that local variables are given unique in different
    stackframes. This is done by attaching the callstack to local variables. *)

open Mopsa
open Sig.Abstraction.Stateless
open Stubs.Ast
open Ast


module Domain =
struct

  (******************)
  (** Domain header *)
  (******************)

  include GenStatelessDomainId(struct
      let name = "c.memory.stackframe"
    end)

  let checks = []

  (************************)
  (** Auxiliary variables *)
  (************************)

  (** This vkind is used to attach the callstack to local variables *)
  type var_kind += V_c_stack_var of callstack * var

  (** Create a stack variable *)
  let mk_stack_var cs v =
    let uniq_name = Format.asprintf "stack(%a, %s)" pp_callstack cs v.vname in
    mkv uniq_name (V_c_stack_var (cs, v)) v.vtyp

  let () = register_var {
      print = (fun next fmt v ->
          match vkind v with
          | V_c_stack_var (cs, vv) -> pp_var fmt vv
          | _ -> next fmt v
        );
      compare = (fun next v1 v2 ->
          match vkind v1, vkind v2 with
          | V_c_stack_var (cs1, vv1), V_c_stack_var (cs2, vv2) ->
            Compare.pair compare_callstack compare_var
              (cs1, vv1) (cs2, vv2)
          | _ ->
            next v1 v2
        );
    }

  (**********************)
  (** Utility functions *)
  (**********************)

  (** Check whether a variable is a local/parameter variable of the current
      function *)
  let is_local_variable v flow =
    let cs = Flow.get_callstack flow in
    match cs, vkind v with
    | hd :: _, V_cvar info -> (
        let f = find_c_fundec_by_name hd.call_fun_orig_name flow in
        match info.cvar_scope with
        | Variable_local f'
        | Variable_parameter f' ->
          f.c_func_org_name = f'.c_func_org_name
        | _ -> false
      )

    | _ -> false


  (** Check whether a variable is a parameter variable of the current
      function *)
  let is_parameter_variable v flow =
    let cs = Flow.get_callstack flow in
    match cs, vkind v with
    | hd :: _, V_cvar info -> (
        let f = find_c_fundec_by_name hd.call_fun_orig_name flow in
        match info.cvar_scope with
        | Variable_parameter f' ->
          f.c_func_org_name = f'.c_func_org_name
        | _ -> false
      )

    | _ -> false


  (** Check whether a variable is a local/parameter variable of the caller
      function *)
  let is_caller_variable v flow =
    let cs = Flow.get_callstack flow in
    match cs, vkind v with
    | _ :: c :: _, V_cvar info -> (
        let f = find_c_fundec_by_name c.call_fun_orig_name flow in
        match info.cvar_scope with
        | Variable_local f'
        | Variable_parameter f' ->
          f.c_func_org_name = f'.c_func_org_name
        | _ -> false
      )

    | _ -> false


  (** Check whether an expression contains a local variable of the caller
      function *)
  let is_caller_expr e flow =
    exists_expr
      (fun ee ->
         match ekind ee with
         | E_var (v, _) -> is_caller_variable v flow
         | _ -> false
      )
      (fun s -> false)
      e

  (** Rewrite an expression by replacing local variables with their stack version *)
  let rewrite_expr e flow =
    let cs = Flow.get_callstack flow in
    map_expr
      (fun ee ->
         match ekind ee with
         | E_var({vkind = V_c_stack_var _}, _) ->
           Keep ee

         | E_var(v, mode) when is_c_type v.vtyp && is_local_variable v flow ->
           let vv = mk_stack_var cs v in
           Keep (mk_var vv ~mode ee.erange)

         | _ -> VisitParts ee
      )
      (fun s -> VisitParts s)
      e

  (***********************)
  (** Transfer functions *)
  (***********************)

  (** Declare a local variable *)
  let exec_declare v init scope range man flow =
    let vv = mk_stack_var (Flow.get_callstack flow) v in
    man.exec (mk_c_declaration vv init scope range) ~route:(Below name) flow

  (** Add a local variable to the environment *)
  let exec_add v range man flow =
    let vv = mk_stack_var (Flow.get_callstack flow) v in
    man.exec (mk_add_var vv range) ~route:(Below name) flow

  (** Remove a local variable from the environment *)
  let exec_remove v range man flow =
    let vv = mk_stack_var (Flow.get_callstack flow) v in
    man.exec (mk_remove_var vv range) ~route:(Below name) flow

  (** Rename a local variable *)
  let exec_rename v1 v2 range man flow =
    let cs = Flow.get_callstack flow in
    let vv1 = mk_stack_var cs v1 in
    let vv2 = mk_stack_var cs v2 in
    man.exec (mk_rename_var vv1 vv2 range) ~route:(Below name) flow

  (** Expand a local variable *)
  let exec_expand v1 vars range man flow =
    let cs = Flow.get_callstack flow in
    let vv1 = mk_stack_var cs v1 in
    man.exec (mk_expand (mk_var vv1 range) vars range) ~route:(Below name) flow

  (** Assign to a local variable *)
  let exec_assign lval e range man flow =
    let lval' = rewrite_expr lval flow in
    (* Distinguish the case when a parameter is initialized with the call
       argument. In this case, the argument expression is defined in the
       previous stackframe. *)
    let e' =
      match ekind lval with
      | E_var(v, _)
        when is_parameter_variable v flow
          && is_caller_expr e flow ->
        (* Set previous stackframe *)
        let cs = Flow.get_callstack flow in
        let cs' = List.tl cs in
        let flow' = Flow.set_callstack cs' flow in
        rewrite_expr e flow'

      | _ ->
        rewrite_expr e flow
    in
    man.exec (mk_assign lval' e' range) ~route:(Below name) flow

  (** Evaluate a local variable *)
  let eval_var v mode range man flow =
    let cs = Flow.get_callstack flow in
    let vv = mk_stack_var cs v in
    man.eval (mk_var vv ~mode range) ~route:(Below name) flow

  (** Evaluate &e *)
  let eval_address_of e range man flow =
    let e = rewrite_expr e flow in
    man.eval (mk_c_address_of e range) ~route:(Below name) flow

  (** Evaluate quantified formulas *)
  let eval_stub_quantified_formula quants body typ range man flow =
    let cs = Flow.get_callstack flow in
    let quants' =
      List.map
        (fun (quant, var, set) ->
           let var' =
             if is_local_variable var flow then
               mk_stack_var cs var
             else
               var
           in
           let set' =
             match set with
             | S_interval(lo, hi) ->
               let lo' = rewrite_expr lo flow in
               let hi' = rewrite_expr hi flow in
               S_interval(lo', hi')
             | S_resource _ ->
               set
           in
           (quant, var', set')
        ) quants
    in
    let body' = rewrite_expr body flow in
    man.eval (mk_stub_quantified_formula quants' body' ~etyp:typ range) ~route:(Below name) flow

  (*****************)
  (** Entry points *)
  (*****************)

  let init prog man flow = flow

  let exec stmt man flow =
    match skind stmt with
    | S_c_declaration(v, init, scope) when is_local_variable v flow ->
      exec_declare v init scope stmt.srange man flow |>
      Option.some

    | S_add({ekind = E_var(v, None)})
      when is_c_type v.vtyp && is_local_variable v flow ->
      exec_add v stmt.srange man flow |>
      Option.some

    | S_remove({ekind = E_var(v, None)})
      when is_c_type v.vtyp && is_local_variable v flow ->
      exec_remove v stmt.srange man flow |>
      Option.some

    | S_rename({ekind = E_var(v1, None)}, {ekind = E_var(v2, None)})
      when is_c_type v1.vtyp && is_local_variable v1 flow && is_c_type v2.vtyp ->
      exec_rename v1 v2 stmt.srange man flow |>
      Option.some

    | S_expand({ekind = E_var(v1, None)}, vars)
      when is_c_type v1.vtyp && is_local_variable v1 flow ->
      exec_expand v1 vars stmt.srange man flow |>
      Option.some

    | S_assign(lval, e) when is_c_type lval.etyp ->
      exec_assign lval e stmt.srange man flow |>
      Option.some

    | _ -> None

  let eval exp man flow =
    match ekind exp with
    | E_var({vkind = V_c_stack_var _ }, _) ->
      None

    | E_var(v, mode) when is_c_type v.vtyp && is_local_variable v flow ->
      eval_var v mode exp.erange man flow |>
      Option.some

    | E_c_address_of e ->
      eval_address_of e exp.erange man flow |>
      Option.some

    | E_stub_quantified_formula(quants, body) ->
      eval_stub_quantified_formula quants body exp.etyp exp.erange man flow |>
      Option.some

    | _ -> None

  let ask query man flow  = None

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
