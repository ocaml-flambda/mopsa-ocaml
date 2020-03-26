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
open Framework.Core.Sig.Domain.Intermediate
open Universal.Ast
open Stubs.Ast
open Ast
open Zone
open Common.Points_to
open Common.Scope_update
open Common.Builtins
open Universal.Iterators.Interproc.Common


module Domain =
struct


  (** {2 Abstract state} *)
  (** ****************** *)

  (* The domain stores the set of addresses allocated by alloca *)
  module AddrSet = Framework.Lattices.Powerset.Make(Addr)

  include AddrSet


  (** {2 Domain header} *)
  (** ================= *)

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.iterators.interproc"
    end)


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

  let alarms = []

  (** {2 Lattice operators} *)
  (** ********************* *)

  let is_bottom _ = false

  let print fmt (a:t) =
    Format.fprintf fmt "alloca: %a@\n" AddrSet.print a

  let merge pre (a1,log1) (a2,log2) =
    join a1 a2


  (** {2 Initialization of environments} *)
  (** ================================== *)

  let init prog man flow = set_env T_cur empty man flow


  (** {2 Computation of post-conditions} *)
  (** ================================== *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_c_return (Some e,upd) ->
      let ret = Context.find_unit return_key (Flow.get_ctx flow) in
      let rrange = get_last_call_site flow in
      let flow =
        man.exec (mk_add_var ret rrange) flow |>
        man.exec (mk_assign (mk_var ret rrange) e rrange) |>
        update_scope upd rrange man
      in
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (stmt.srange, true)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> OptionExt.return

    | S_c_return (None,upd) ->
      let rrange = get_last_call_site flow in
      let flow = update_scope upd rrange man flow in
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (stmt.srange, false)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> OptionExt.return


    | _ -> None


  (** {2 Evaluation of expressions} *)
  (** ============================= *)

  (** Check if there is a recursive call to a function *)
  let is_recursive_call f flow =
    let open Callstack in
    let cs = Flow.get_callstack flow in
    List.exists (fun c -> c.call_fun_uniq_name = f.c_func_unique_name) cs


  (** 𝔼⟦ alloca(size) ⟧ *)
  let eval_alloca_call size range man flow =
    (* allocate a resource *)
    man.eval (mk_stub_alloc_resource "alloca" range) flow >>$ fun e flow ->
    match ekind e with
    | E_addr addr ->
      (* add the resource to local state *)
      let flow = map_env T_cur (add addr) man flow in
      (* add the address to memory state *)
      man.post (mk_add e range) flow >>$ fun () flow ->
      (* set the size of the resource *)
      let cond = mk_binop (mk_stub_builtin_call BYTES e ~etyp:ul range) O_eq size ~etyp:T_bool range in
      man.post (mk_assume cond range) flow >>$ fun () flow ->
      Eval.singleton e flow

    | _ -> assert false


  (** Eval a function call *)
  let eval_call fundec args range man flow =
    if fundec.c_func_org_name = "__builtin_alloca" then
      match args with
      | [size] -> eval_alloca_call size range man flow
      | _ -> panic_at range "invalid call to alloca"
    else
    if is_builtin_function fundec.c_func_org_name
    then
      let exp' = mk_expr (E_c_builtin_call(fundec.c_func_org_name, args)) ~etyp:fundec.c_func_return range in
      man.eval ~zone:(Zone.Z_c, Zone.Z_c_low_level) exp' flow
    else
      (* save the alloca resources of the caller before resetting it *)
      let caller_alloca_addrs = get_env T_cur man flow in
      let flow = set_env T_cur empty man flow in
      let ret =
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
            fun_orig_name = fundec.c_func_org_name;
            fun_uniq_name = fundec.c_func_unique_name;
            fun_parameters = fundec.c_func_parameters;
            fun_locvars = [];
            (* FIXME: This is a temporary fix to avoid double removal of
               local variables. The field fun_locvars is used by the
               Universal iterator at the end of the call to clean the
               environment. Since the environment is automatically
               cleaned by the scope mechanism, local variables are
               removed twice. *)
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
      in
      (* free alloca addresses *)
      ret >>$ fun e flow ->
      let callee_alloca_addrs = get_env T_cur man flow in
      let flow = set_env T_cur caller_alloca_addrs man flow in
      AddrSet.fold
        (fun addr acc -> Post.bind (man.post (mk_stub_free (mk_addr addr range) range)) acc)
        callee_alloca_addrs (Post.return flow)
      >>$ fun () flow ->
      Eval.singleton e flow


  let eval zone exp man flow =
    match ekind exp with
    | E_call({ ekind = E_c_function { c_func_variadic = true}}, args) ->
      None

    | E_call({ ekind = E_c_function f}, args) ->
      eval_call f args exp.erange man flow |>
      OptionExt.return

    | E_call(f, args) ->
      man.eval ~zone:(Zone.Z_c, Z_c_points_to) f flow >>$? fun ff flow ->

      begin match ekind ff with
        | E_c_points_to (P_fun f) ->
          eval_call f args exp.erange man flow |>
          OptionExt.return

        | _ ->
          Soundness.warn_at exp.erange
            "ignoring side-effect of undetermined function pointer %a"
            pp_expr f
          ;
          if is_c_void_type exp.etyp then
            Eval.empty_singleton flow |>
            OptionExt.return
          else
            Eval.singleton (mk_top exp.etyp exp.erange) flow |>
            OptionExt.return
      end

    | _ -> None



  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

  let refine _ _ _ = assert false

end

let () =
  Framework.Core.Sig.Domain.Intermediate.register_domain (module Domain)
