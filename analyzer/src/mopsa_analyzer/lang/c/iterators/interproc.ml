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
open Sig.Abstraction.Domain
open Universal.Ast
open Stubs.Ast
open Ast
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

  let checks = []


  (** {2 Lattice operators} *)
  (** ********************* *)

  let is_bottom _ = false

  let widen ctx = join

  let merge pre (a1,e1) (a2,e2) =
    join a1 a2


  (** {2 Initialization of environments} *)
  (** ================================== *)

  let init prog man flow = set_env T_cur empty man flow

  (************************)
  (** Auxiliary variables *)
  (************************)

  (** This vkind is used to attach the callstack to local variables *)
  type var_kind += V_c_stack_var of callstack * var

  (** Create a stack variable *)
  let mk_stack_var cs v =
    let uniq_name = Format.asprintf "stack(%a, %s)" pp_callstack_short cs v.vname in
    mkv uniq_name (V_c_stack_var (cs, v)) v.vtyp

  let () = register_var {
      print = (fun next fmt v ->
          match vkind v with
          | V_c_stack_var (cs, vv) -> Format.fprintf fmt "stack(%a, %a)" pp_callstack_short cs pp_var vv
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

  (** {2 Computation of post-conditions} *)
  (** ================================== *)

  let exec stmt man flow =
    match skind stmt with
    | S_c_return (r,upd) ->
      (* Let first Universal manage the return flow *)
      man.exec (mk_stmt (S_return r) stmt.srange) flow >>%? fun flow ->
      (* Now clean the post-state using scope updater *)
      (* To do that, first move the return environment to cur *)
      let flow = Flow.copy (T_return stmt.srange) T_cur man.lattice flow flow in
      (* Now clean cur *)
      update_scope upd stmt.srange man flow >>%? fun flow ->
      (* Finally, move cur to return flow *)
      let cur = Flow.get T_cur man.lattice flow in
      Flow.set (T_return (stmt.srange)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> OptionExt.return

    | _ -> None


  (** {2 Evaluation of expressions} *)
  (** ============================= *)

  (** Check if there is a recursive call to a function *)
  let is_recursive_call f range flow =
     (* let open Callstack in
      * let cs = Flow.get_callstack flow in
      * List.exists (fun c -> c.call_fun_uniq_name = f.c_func_unique_name) cs *)
    let f_orig = f.c_func_org_name in
    let f_uniq = f.c_func_unique_name in
    Universal.Iterators.Interproc.Common.check_recursion f_orig f_uniq range
    (Callstack.push_callstack f_orig ~uniq:f_uniq range (Flow.get_callstack flow))

  (** Rename the variables of a function's body by attaching the callstack to them *)
  let rename_function_variables_body cs body =
    let is_local_scope = function
      | Variable_local _ | Variable_parameter _ ->
        true
      | _ ->
        false
    in
    let rec visit_expr e =
      map_expr
        (fun e ->
           match ekind e with
           | E_var(v, mode) ->
             begin match vkind v with
               | V_cvar cv when is_local_scope cv.cvar_scope ->
                 let v' = mk_stack_var cs v in
                 let e' = { e with ekind = E_var(v', mode) } in
                 Keep e'
               | _ ->
                 Keep e
             end
           | _ ->
             VisitParts e
        )
        (fun s ->
           let s' = visit_stmt s in
           Keep s'
        ) e
    and visit_stmt s =
      map_stmt
        (fun e ->
           let e' = visit_expr e in
           Keep e'
        )
        (fun s ->
           match skind s with
           | S_c_declaration(v, init, scope) when is_local_scope scope ->
             let v' = mk_stack_var cs v in
             let init' = OptionExt.lift visit_init init in
             let s' = { s with skind = S_c_declaration(v', init', scope) } in
             Keep s'
           | S_block(block, vars) ->
             let block' = List.map visit_stmt block in
             let vars' = List.map (mk_stack_var cs) vars in
             let s' = { s with skind = S_block(block', vars') } in
             Keep s'
           | _ ->
             VisitParts s
        ) s
    and visit_init = function
      | C_init_expr e ->
        C_init_expr (visit_expr e)
      | C_init_list(inits, filler) ->
        C_init_list(List.map visit_init inits, OptionExt.lift visit_init filler)
      | C_init_implicit t ->
        C_init_implicit t
    in
    visit_stmt body

  (** Renamve local variables in a stub body by attaching the callstack to them *)
  let rename_function_variables_stub cs stub =
    assert false

  (** Rename the variables of a function by attaching the callstack to them *)
  let rename_function_variables cs fundec =
    if is_empty_callstack cs then
      fundec
    else
      { fundec with
        c_func_parameters = List.map (mk_stack_var cs) fundec.c_func_parameters;
        c_func_local_vars = List.map (mk_stack_var cs) fundec.c_func_local_vars;
        c_func_body = OptionExt.lift (rename_function_variables_body cs) fundec.c_func_body;
        c_func_stub = OptionExt.lift (rename_function_variables_stub cs) fundec.c_func_stub;
      }

  (** 𝔼⟦ alloca(size) ⟧ *)
  let eval_alloca_call size range man flow =
    (* allocate a resource *)
    man.eval (mk_stub_alloc_resource "alloca" range) flow >>$ fun e flow ->
    match ekind e with
    | E_addr (addr, _) ->
      (* add the resource to local state *)
      let flow = map_env T_cur (add addr) man flow in
      (* add the address to memory state *)
      man.exec (mk_add e range) flow >>% fun flow ->
      (* set the size of the resource *)
      let cond = mk_binop (mk_stub_builtin_call BYTES [e] ~etyp:ul range) O_eq size ~etyp:T_bool range in
      man.exec (mk_assume cond range) flow >>% fun flow ->
      Eval.singleton e flow

    | _ -> assert false


  (** Evaluate arguments containing function calls *)
  let rec eval_calls_in_args args man flow =
    match args with
    | [] -> Cases.singleton [] flow
    | arg::tl ->
      if Visitor.exists_expr
          (fun e -> match ekind e with E_call _ -> true | _ -> false)
          (fun s -> false) arg
      then
        (* Evaluating arguments may result in disjunctions.
           To avoid calling the function several times, we assign the call
           to a temporary variable *)
        let tmp = mk_range_attr_var arg.erange "arg" arg.etyp in
        man.exec (mk_add_var tmp arg.erange) flow >>%
        man.exec (mk_assign (mk_var tmp arg.erange) arg arg.erange) >>%
        eval_calls_in_args tl man >>$ fun tl flow ->
        Cases.singleton (mk_var tmp arg.erange :: tl) ~cleaners:[mk_remove_var tmp arg.erange] flow
      else
        eval_calls_in_args tl man flow >>$ fun tl flow ->
        Cases.singleton (arg::tl) flow


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
      man.eval exp' flow
    else
      (* save the alloca resources of the caller before resetting it *)
      let caller_alloca_addrs = get_env T_cur man flow in
      let flow = set_env T_cur empty man flow in
      let ret =
        (* Process arguments by evaluating function calls *)
        eval_calls_in_args args man flow >>$ fun args flow ->
        (* We don't support recursive functions yet! *)
        if is_recursive_call fundec range flow then (
          warn_at range "recursive call on %s, returning top" fundec.c_func_org_name;
          let flow =
            Flow.add_local_assumption
              (Universal.Soundness.A_ignore_recursion_side_effect fundec.c_func_org_name)
              range flow
          in
          if is_c_void_type fundec.c_func_return then
            Eval.singleton (mk_unit range) flow
          else
            man.eval (mk_top fundec.c_func_return range) flow
        )
        else
         let () = debug "not a recursive call" in
         let fundec = rename_function_variables (Flow.get_callstack flow) fundec in
         match fundec with
         | {c_func_body = Some body; c_func_stub = None; c_func_variadic = false} ->
           debug "body = %a" pp_stmt body;
           let open Universal.Ast in
           let ret_var = mktmp ~typ:fundec.c_func_return () in
           let body' =
             if exists_stmt
                 (fun e -> false)
                 (fun s -> match skind s with S_c_goto _ -> true | _ -> false)
                 body
             then
               {skind = S_c_goto_stab (body); srange = tag_range (srange body) "goto-stabilization"}
             else body
           in
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
             fun_body = body';
             fun_return_type = if is_c_void_type fundec.c_func_return then None else Some fundec.c_func_return;
             fun_return_var = ret_var;
             fun_range = fundec.c_func_range;
           }
           in
           let exp' = mk_call fundec' args range in
           man.eval exp' flow ~route:(Below name)

        | {c_func_variadic = true} ->
          let exp' = mk_c_call fundec args range in
          man.eval exp' flow ~route:(Below name)

        | {c_func_stub = Some stub} ->
          let exp' = Stubs.Ast.mk_stub_call stub args range in
          man.eval exp' flow

        | {c_func_body = None; c_func_org_name; c_func_return} ->
          let flow =
            if man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then flow
            else
              let r = bind_list args man.eval flow in
              let flow = r >>$ (fun _ flow -> Post.return flow)|> post_to_flow man in
              let () = warn_at range "%a" pp_assumption_kind (Soundness.A_ignore_undefined_function c_func_org_name) in
              Flow.add_local_assumption
                (Soundness.A_ignore_undefined_function c_func_org_name)
                range flow
          in
          if is_c_void_type c_func_return then
            Eval.singleton (mk_unit range) flow
          else
            man.eval (mk_top c_func_return range) flow
      in
      (* free alloca addresses *)
      ret >>$ fun e flow ->
      let callee_alloca_addrs = get_env T_cur man flow in
      let flow = set_env T_cur caller_alloca_addrs man flow in
      AddrSet.fold
        (fun addr acc -> acc >>% man.exec (mk_stub_free (mk_addr addr range) range))
        callee_alloca_addrs (Post.return flow)
      >>% fun flow ->
      Eval.singleton e flow

  (* 𝔼⟦ *p ⟧ where p is a pointer to a function *)
  let eval_deref_function_pointer p range man flow =
    resolve_pointer p man flow >>$ fun pt flow ->
    match pt with
    | P_fun f ->
      Eval.singleton (mk_expr (E_c_function f) ~etyp:(under_type p.etyp) range) flow

    | P_top ->
      let flow =
        Flow.add_local_assumption
          (Soundness.A_ignore_undetermined_function_pointer p)
          range flow
      in
      if under_type p.etyp |> is_c_void_type then
        Eval.singleton (mk_unit range) flow
      else
        man.eval (mk_top (under_type p.etyp) range) flow

    | _ ->
      panic_at range
             "deref_function_pointer: pointer %a points to a non-function object %a"
             pp_expr p
             pp_points_to pt

  let eval exp man flow =
    match ekind exp with
    | E_call({ ekind = E_c_function { c_func_variadic = true}}, args) ->
      None

    | E_call({ ekind = E_c_function f}, args) ->
      eval_call f args exp.erange man flow |>
      OptionExt.return

    | E_call(f, args) when is_c_type (etyp f) ->
      resolve_pointer f man flow >>$? fun ff flow ->

      begin match ff with
        | P_fun f ->
          eval_call f args exp.erange man flow |>
          OptionExt.return

        | _ ->
          let flow =
            Flow.add_local_assumption
              (Soundness.A_ignore_undetermined_function_pointer f)
              exp.erange flow
          in
          if is_c_void_type exp.etyp then
            Eval.singleton (mk_unit exp.erange) flow |>
            OptionExt.return
          else
            man.eval (mk_top exp.etyp exp.erange) flow |>
            OptionExt.return
      end

    | E_c_deref p when under_type p.etyp |> is_c_function_type
      ->
      eval_deref_function_pointer p exp.erange man flow |>
      OptionExt.return

    | _ -> None



  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None


  (** Pretty printer *)
  (** ============== *)

  let print_state printer (a:t) =
    pprint ~path:[Key "alloca"] printer (pbox AddrSet.print a)

  let print_expr _ _ _ _ = ()

end

let () =
  register_standard_domain (module Domain)
