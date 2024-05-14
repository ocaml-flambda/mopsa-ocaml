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
open Common
open Common.Points_to
open Common.Scope_update
open Common.Builtins
open Universal.Iterators.Interproc.Common



let opt_error_builtin = ref true

let () =
  register_language_option "c" {
    key="-error-is-builtin";
    category = "C";
    doc = "assume error function corresponds to the builtin";
    spec = ArgExt.Bool (fun b -> opt_error_builtin := b);
    default = "true"
  }



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

  let init prog man flow =
    let () =
      if !opt_error_builtin then
        let () = Hashtbl.add Common.Builtins.builtin_functions "error" () in
        Hashtbl.add Common.Builtins.builtin_functions "error_at" ()
    in
    set_env T_cur empty man flow |>
    Option.some


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
    let f_orig = f.c_func_org_name in
    let f_uniq = f.c_func_unique_name in
    let cs = Callstack.push_callstack f_orig ~uniq:f_uniq range (Flow.get_callstack flow) in
    Universal.Iterators.Interproc.Common.check_recursion f_orig f_uniq range cs

  let is_local_scope = function
    | Variable_local _ | Variable_parameter _ ->
      true
    | _ ->
      false

  (** Rename the variables of a function's body by attaching the callstack to them *)
  let rename_variables_in_function_body cs body =
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
           | S_c_return(e, upd) ->
             let e' = OptionExt.lift visit_expr e in
             let upd' = visit_scope_update upd in
             let s' = { s with skind = S_c_return(e', upd') } in
             Keep s'
           | S_c_break upd ->
             let upd' = visit_scope_update upd in
             let s' = { s with skind = S_c_break upd' } in
             Keep s'
           | S_c_continue upd ->
             let upd' = visit_scope_update upd in
             let s' = { s with skind = S_c_continue upd' } in
             Keep s'
           | S_c_goto(label, upd) ->
             let upd' = visit_scope_update upd in
             let s' = { s with skind = S_c_goto(label, upd') } in
             Keep s'
           | S_c_switch_case(es, upd) ->
             let es' = List.map visit_expr es in
             let upd' = visit_scope_update upd in
             let s' = { s with skind = S_c_switch_case(es', upd') } in
             Keep s'
           | S_c_switch_default upd ->
             let upd' = visit_scope_update upd in
             let s' = { s with skind = S_c_switch_default upd' } in
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
    and visit_scope_update upd =
      { c_scope_var_added = List.map (mk_stack_var cs) upd.c_scope_var_added;
        c_scope_var_removed = List.map (mk_stack_var cs) upd.c_scope_var_removed; }
    in
    visit_stmt body

  (** Renamve local variables in a stub body by attaching the callstack to them *)
  let rename_variables_in_stub cs stub =
    let with_range f x =
      bind_range x f
    in
    let rec visit_section = function
      | S_case case -> S_case (visit_case case)
      | S_leaf leaf -> S_leaf (visit_leaf leaf)
    and visit_case case =
      { case with
        case_body    = List.map visit_leaf case.case_body;
        case_locals  = List.map (with_range visit_local) case.case_locals;
        case_assigns = List.map (with_range visit_assigns) case.case_assigns; }
    and visit_leaf = function
      | S_local local       -> S_local (with_range visit_local local)
      | S_assumes assumes   -> S_assumes (with_range visit_formula assumes)
      | S_requires requires -> S_requires (with_range visit_formula requires)
      | S_assigns assigns   -> S_assigns (with_range visit_assigns assigns)
      | S_ensures ensures   -> S_ensures (with_range visit_formula ensures)
      | S_free free         -> S_free (with_range visit_expr free)
      | S_message _ as x    -> x
    and visit_local local =
      { lvar = mk_stack_var cs local.lvar;
        lval = visit_local_value local.lval; }
    and visit_local_value = function
      | L_new _ as x -> x
      | L_call(f, args) ->
        L_call(visit_expr f, List.map visit_expr args)
    and visit_assigns assigns =
      { assign_target = visit_expr assigns.assign_target;
        assign_offset = List.map visit_interval assigns.assign_offset; }
    and visit_interval (lo, hi) =
      (visit_expr lo, visit_expr hi)
    and visit_expr e =
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
        (fun s -> VisitParts s)
        e
    and visit_formula f =
      with_range (function
          | F_expr e            -> F_expr (visit_expr e)
          | F_binop(op, f1, f2) -> F_binop(op, visit_formula f1, visit_formula f2)
          | F_not f             -> F_not (visit_formula f)
          | F_forall(v, s, f)   -> F_forall(mk_stack_var cs v, visit_set s, visit_formula f)
          | F_exists(v, s, f)   -> F_exists(mk_stack_var cs v, visit_set s, visit_formula f)
          | F_in(e, s)          -> F_in(visit_expr e, visit_set s)
          | F_otherwise(f, e)   -> F_otherwise(visit_formula f, visit_expr e)
          | F_if(f1, f2, f3)    -> F_if(visit_formula f1, visit_formula f2, visit_formula f3)
        ) f
    and visit_set = function
      | S_interval i      -> S_interval (visit_interval i)
      | S_resource _ as x -> x
    in
    { stub with
      stub_func_body = List.map visit_section stub.stub_func_body;
      stub_func_params = List.map (mk_stack_var cs) stub.stub_func_params;
      stub_func_locals = List.map (with_range visit_local) stub.stub_func_locals;
      stub_func_assigns = List.map (with_range visit_assigns) stub.stub_func_assigns; }

  (** Rename the variables of a function by attaching the callstack to them *)
  let rename_variables_in_fundec cs fundec =
    (* No renaming the function is not in the callstack *)
    let first_call =
      cs |> List.for_all
        (fun c -> c.call_fun_uniq_name <> fundec.c_func_unique_name)
    in
    if first_call then
      false, fundec
    else
      true, { fundec with
        c_func_parameters = List.map (mk_stack_var cs) fundec.c_func_parameters;
        c_func_local_vars = List.map (mk_stack_var cs) fundec.c_func_local_vars;
        c_func_body = OptionExt.lift (rename_variables_in_function_body cs) fundec.c_func_body;
        c_func_stub = OptionExt.lift (rename_variables_in_stub cs) fundec.c_func_stub;
      }

  (** 𝔼⟦ alloca(size) ⟧ *)
  let eval_alloca_call size range man flow =
    (* allocate a resource *)
    man.eval (mk_stub_alloc_resource "alloca" range) flow >>$ fun e flow ->
    match ekind e with
    | E_addr (addr, _) ->
      (* add the resource to local state *)
      map_env T_cur (add addr) man flow >>% fun flow ->
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

  let is_c_fun_boolean_predicate fundec args man flow =
    List.length args = 1 &&
    begin match ekind (List.hd args) with
    | E_binop(op, e1, e2) ->
      is_comparison_op op || op = O_c_and || op = O_c_or
    | E_unop(op, _) -> op = O_log_not
    | _ -> false
    end &&
    begin match fundec with
      | {c_func_body = Some body; c_func_variadic = false } ->
        let stmts_count = Visitor.fold_stmt (fun c e -> Keep c)
            (fun c s -> VisitParts (c+1)) 0 body in
        debug "stmts_count = %d" stmts_count;
        stmts_count < 20
      | _ -> false
    end

  let is_c_constant e =
    match ekind e with
    | E_constant c -> is_c_int_type (etyp e)
    | _ -> false

  (** Eval a function call *)
  let eval_call fundec args range man flow =
    let fundec_has_been_modified = ref false in 
    if List.length args > 0 && List.length fundec.c_func_parameters > 0 then
      debug "%s %a %a" fundec.c_func_org_name pp_typ (List.hd fundec.c_func_parameters).vtyp pp_typ (List.hd args).etyp;
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
      (* in the case f(cond) where cond is a boolean, f is not too difficult,
         we rewrite it into if(cond) f(1) else f(0) to gain precision *)
    if is_c_fun_boolean_predicate fundec args man flow && not @@ is_c_constant @@ List.hd args then
      let arg = List.hd args in
      let arg_type = etyp arg in
      let mk_call c = mk_expr (E_call (mk_expr (E_c_function fundec) range, [c]))  ~etyp:fundec.c_func_return range in
      let exp' = mk_expr ~etyp:fundec.c_func_return
          (E_c_conditional
             (arg,
              mk_call (mk_one ~typ:arg_type range),
              mk_call (mk_zero ~typ:arg_type range))) range in
      let () = debug "%s is considered as a boolean predicate, rewriting as %a" fundec.c_func_org_name pp_expr exp' in
      man.eval exp' flow
    else
      (* save the alloca resources of the caller before resetting it *)
      let caller_alloca_addrs = get_env T_cur man flow in
      (* if the environment is paritiotione, we need to collapse addresses from
       * all paritions, since we can't keep a relation between the partitions
       * before calling the function and the paritions after the return *)
      let caller_alloca_addrs = Cases.reduce_result
          (fun addrs _ -> addrs) caller_alloca_addrs 
          ~join:AddrSet.join
          ~meet:AddrSet.meet
          ~bottom:(fun () -> AddrSet.bottom)
      in
      (* Empty alloca before the call. Note that we can't use the bind operator
       * [>>%] here to avoid calling the body of the function in every
       * partition *)
      let flow = set_env_flow T_cur empty man flow in
      let ret =
        (* Process arguments by evaluating function calls *)
        eval_calls_in_args args man flow >>$ fun args flow ->
        if is_recursive_call fundec range flow then (
          warn_at range "recursive call on %s, returning top" fundec.c_func_org_name;
          let flow =
            Flow.add_local_assumption
              (Universal.Iterators.Interproc.Common.A_ignore_recursion_side_effect fundec.c_func_org_name)
              range flow
          in
          if is_c_void_type fundec.c_func_return then
            Eval.singleton (mk_unit range) flow
          else
            man.eval (mk_top fundec.c_func_return range) flow
        )
        else
         let is_modified, fundec = rename_variables_in_fundec (Flow.get_callstack flow) fundec in
         let () = fundec_has_been_modified := is_modified in 
         let flow =
           if !fundec_has_been_modified then 
           (* after fundec renaming, we save the updated program in the context for the interactive engine *)
             let old_c_program = get_c_program flow in 
             let new_c_program =
               {old_c_program with
                c_functions = fundec :: old_c_program.c_functions } in
             set_c_program new_c_program flow
           else flow in
         match fundec with
         | {c_func_body = Some body; c_func_stub = None; c_func_variadic = false} ->
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

        | {c_func_stub = Some stub} ->
          let exp' = Stubs.Ast.mk_stub_call stub args range in
          man.eval exp' flow >>$ fun exp' flow ->
          let flow =
            if List.mem fundec.c_func_org_name ["sqrt"; "sqrtf"; "sqrtl"] then
              man.exec (mk_assume (eq ~etyp:T_bool exp' (mk_unop O_sqrt ~etyp:fundec.c_func_return (List.hd args) range) range) range) flow
            else Post.return flow in
          Eval.singleton exp' (post_to_flow man flow)

        | {c_func_variadic = true} ->
          let exp' = mk_c_call fundec args range in
          man.eval exp' flow ~route:(Below name)

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
      let flow =
        if !fundec_has_been_modified then
          (* we updated the program before analyzing the renamed function, we can now get rid of it *)
          let c_program = get_c_program flow in
          set_c_program {c_program with c_functions = List.tl c_program.c_functions} flow
        else flow in
      get_env T_cur man flow >>$ fun callee_addrs flow ->
      let flow = set_env_flow T_cur caller_alloca_addrs man flow in
      AddrSet.fold
        (fun addr acc -> acc >>% man.exec (mk_stub_free (mk_addr addr range) range))
        callee_addrs (Post.return flow)
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

    | P_null ->
      Common.Alarms.raise_c_null_deref_alarm p man flow |> Eval.empty 

    | _ ->
      panic_at range
             "deref_function_pointer: pointer %a points to a non-function object %a"
             pp_expr p
             pp_points_to pt

  let eval exp man flow =
    match ekind exp with
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
