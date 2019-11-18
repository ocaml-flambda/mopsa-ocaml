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

(** Abstraction of C function calls *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Universal.Ast
open Ast
open Zone
open Common.Points_to
open Common.Scope_update
open Universal.Iterators.Interproc.Common

module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.iterators.interproc"
    end)


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [Z_c];
      uses = []
    };

    ieval = {
      provides = [Z_c, Z_c_low_level];
      uses = [
        Z_c, Z_c_points_to;
        Universal.Zone.Z_u, any_zone;
        Stubs.Zone.Z_stubs, Z_any
      ]
    }
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_c_return (Some e,upd) ->
      let range = stmt.srange in
      let ret, rrange = Context.find_unit return_key (Flow.get_ctx flow) in
      let flow =
        man.exec (mk_add_var ret rrange) flow |>
        man.exec (mk_assign (mk_var ret rrange) e range) |>
        update_scope upd range man
      in
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (range, true)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> Option.return

    | S_c_return (None,upd) ->
      let range = stmt.srange in
      let flow = update_scope upd range man flow in
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (range, false)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> Option.return


    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  (** Check if there is a recursive call to a function *)
  let is_recursive_call f flow =
    let open Callstack in
    let cs = Flow.get_callstack flow in
    List.exists (fun c -> c.call_fun = f.c_func_unique_name) cs


  (** Eval a function call *)
  let eval_call fundec args range man flow =
    if Libs.Builtins.is_builtin_function fundec.c_func_org_name
    then
      let exp' = mk_expr (E_c_builtin_call(fundec.c_func_org_name, args)) ~etyp:fundec.c_func_return range in
      man.eval ~zone:(Zone.Z_c, Zone.Z_c_low_level) exp' flow
    else
      match fundec with
      | _ when is_recursive_call fundec flow ->
        Soundness.warn_at range "ignoring recursive call of function %s in %a" fundec.c_func_org_name pp_range range;
        if is_c_void_type fundec.c_func_return then
          Eval.empty_singleton flow
        else
          Eval.singleton (mk_top fundec.c_func_return range) flow

      | {c_func_body = Some body; c_func_stub = None; c_func_variadic = false} ->
        let open Universal.Ast in
        let ret_var = mktmp ~typ:fundec.c_func_return () in
        let fundec' = {
          fun_name = fundec.c_func_unique_name;
          fun_parameters = fundec.c_func_parameters;
          fun_locvars = fundec.c_func_local_vars;
          fun_body = {skind = S_c_goto_stab (body); srange = srange body};
          fun_return_type = if is_c_void_type fundec.c_func_return then None else Some fundec.c_func_return;
          fun_return_var = ret_var;
          fun_range = fundec.c_func_range;
        }
        in
        let exp' = mk_call fundec' args range in
        (* Universal will evaluate the call into a temporary variable containing the returned value *)
        man.eval ~zone:(Universal.Zone.Z_u, any_zone) exp' flow

      | {c_func_variadic = true} ->
        let exp' = mk_c_call fundec args range in
        man.eval ~zone:(Zone.Z_c, Zone.Z_c_low_level) exp' flow

      | {c_func_stub = Some stub} ->
        let exp' = Stubs.Ast.mk_stub_call stub args range in
        man.eval ~zone:(Stubs.Zone.Z_stubs, any_zone) exp' flow

      | {c_func_body = None; c_func_org_name; c_func_return} ->
        Soundness.warn_at range "ignoring side effects of calling undefined function %s" c_func_org_name;
        if is_c_void_type c_func_return then
          Eval.empty_singleton flow
        else
          Eval.singleton (mk_top c_func_return range) flow


  let eval zone exp man flow =
    match ekind exp with
    | E_call({ ekind = E_c_function { c_func_variadic = true}}, args) ->
      None

    | E_call({ ekind = E_c_function f}, args) ->
      eval_call f args exp.erange man flow |>
      Option.return

    | E_call(f, args) ->
      man.eval ~zone:(Zone.Z_c, Z_c_points_to) f flow >>$? fun ff flow ->

      begin match ekind ff with
        | E_c_points_to (P_fun f) ->
          eval_call f args exp.erange man flow |>
          Option.return

        | _ ->
          Soundness.warn_at exp.erange
            "ignoring side-effect of undetermined function pointer %a"
            pp_expr f
          ;
          if is_c_void_type exp.etyp then
            Eval.empty_singleton flow |>
            Option.return
          else
            Eval.singleton (mk_top exp.etyp exp.erange) flow |>
            Option.return
      end

    | _ -> None



  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
