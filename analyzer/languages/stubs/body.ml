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

(** Inter-procedural iterator of stubs by inlining. *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Alarms

(******************)
(** Trace markers *)
(******************)

type marker += M_stub_case of stub_func * case

let () = register_marker {
    marker_print = (fun next fmt -> function
        | M_stub_case(stub, case) ->
          Format.fprintf fmt "stub-case (%s.%s)" stub.stub_func_name case.case_label
        | m ->
          next fmt m
      );
    marker_compare = (fun next m1 m2 ->
        match m1, m2 with
        | M_stub_case(stub1, case1), M_stub_case(stub2, case2) ->
          Compare.pair String.compare String.compare
            (stub1.stub_func_name, case1.case_label)
            (stub2.stub_func_name, case2.case_label)
        | _ ->
          next m1 m2
      );
    marker_name = (fun next -> function
        | M_stub_case _ -> "stub-case"
        | m -> next m
      );
  }

(********************)
(** Abstract domain *)
(********************)

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "stubs.iterators.body"
    end)

  let checks = [CHK_STUB_CONDITION]


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None


  (** {2 Command-line options} *)
  (** ************************ *)

  let opt_stub_ignored_cases : string list ref = ref []
  (** List of ignored stub cases *)

  let () = register_builtin_option {
      key      = "-stub-ignore-case";
      doc      = " list of stub cases to ignore";
      category = "Stubs";
      spec     = ArgExt.Set_string_list opt_stub_ignored_cases;
      default  = "";
    }

  (** Check whether a case is ignored *)
  let is_case_ignored stub case : bool =
    match stub with
    | None -> false
    | Some s -> List.mem (s.stub_func_name ^ "." ^ case.case_label) !opt_stub_ignored_cases


  (** Evaluation of expressions *)
  (** ========================= *)

  (** Negate a formula *)
  let rec negate_formula (f:formula with_range) : formula with_range =
    match f.content with
    | F_expr e ->
      with_range (F_expr (mk_not e e.erange)) f.range

    | F_binop (IMPLIES, f1, f2) ->
      with_range (
        F_binop (
          AND,
          f1,
          negate_formula f2
        )) f.range

    | F_binop (op, f1, f2) ->
     with_range (F_binop (
         negate_log_binop op,
         negate_formula f1,
         negate_formula f2
       )) f.range

    | F_not f -> f

    | F_forall (var, set, ff) ->
      with_range (F_exists (
          var,
          set,
          negate_formula ff
        )) f.range

    | F_exists (var, set, ff) ->
      with_range (F_forall (
          var,
          set,
          negate_formula ff
        )) f.range

    | F_in (e, S_interval(l,u)) ->
      with_range (
        F_expr (
          mk_binop
            (mk_binop e O_lt l ~etyp:T_bool f.range)
            O_log_or
            (mk_binop e O_gt u ~etyp:T_bool f.range)
            ~etyp:T_bool
            f.range
        )
      ) f.range

    | F_in (e, S_resource res) ->
      with_range (
        F_expr (
          mk_not (mk_stub_resource_mem e res f.range) f.range
        )
      ) f.range

    | F_otherwise _ -> panic_at f.range "negation of 'otherwise' formulas not possible"

    | F_if(c,f1,f2) ->
      with_range (F_if (c, negate_formula f1, negate_formula f2)) f.range

  (** Translate a formula into prenex normal form *)
  let rec formula_to_prenex f =
    match f.content with
    | F_expr cond ->
      [], cond

    | F_binop (AND, f1, f2) ->
      let quants1,cond1 = formula_to_prenex f1 in
      let quants2,cond2 = formula_to_prenex f2 in
      quants1@quants2, mk_log_and cond1 cond2 f.range

    | F_binop (OR, f1, f2) ->
      let quants1,cond1 = formula_to_prenex f1 in
      let quants2,cond2 = formula_to_prenex f2 in
      quants1@quants2, mk_log_or cond1 cond2 f.range

    | F_binop (IMPLIES, f1, f2) ->
      let quants1,cond1 = formula_to_prenex (negate_formula f1) in
      let quants2,cond2 = formula_to_prenex f2 in
      quants1@quants2, mk_log_or cond1 cond2 f.range

    | F_not ff ->
      formula_to_prenex (negate_formula ff)

    | F_in (e, S_interval (l, u)) ->
      [], mk_in e l u f.range

    | F_in (e, S_resource res) ->
      [], mk_stub_resource_mem e res f.range

    | F_forall(v,s,ff) ->
      let quants,cond = formula_to_prenex ff in
      (FORALL,v,s)::quants, cond

    | F_exists(v,s,ff) ->
      let quants,cond = formula_to_prenex ff in
      (EXISTS,v,s)::quants, cond

    | F_otherwise(ff, e) ->
      let quants,cond = formula_to_prenex ff in
      quants, mk_stub_otherwise cond (Some e) f.range

    | F_if(c,f1,f2) ->
      let quants,cond = formula_to_prenex c in
      let quants1,cond1 = formula_to_prenex f1 in
      let quants2,cond2 = formula_to_prenex f2 in
      quants@quants1@quants2, mk_stub_if cond cond1 cond2 f.range


  (* Function to get variables in a condition (and avoid the alarm expression in `otherwise` *)
  let rec vars_of_condition cond =
    fold_expr
      (fun acc e ->
         match ekind e with
         | E_var (v,_) -> Keep (VarSet.add v acc)
         | E_stub_otherwise(ee, _) -> Keep (VarSet.union (vars_of_condition ee) acc)
         | _ -> VisitParts acc
      )
      (fun acc s -> assert false)
      VarSet.empty cond

  (* Check if a variable is used in an expression *)
  let var_in_expr v e =
    exists_expr
      (fun ee ->
         match ekind ee with
         | E_var (vv,_) -> compare_var v vv = 0
         | _ -> false
      )
      (fun s -> false)
      e

  (* Function to remove unnecessary quantifiers not used in an expression *)
  let remove_unnecessary_quantifiers quants e =
    let vars = vars_of_condition e in
    (* A quantified var is necessary if (i) it used in the condition, or (ii)
       used in the bounds of an other necessary quantifier *)
    let quants =
      List.map
        (fun ((_,v,_) as q) -> (q, VarSet.mem v vars))
        quants
    in
    let rec iter = function
      | [] -> []
      | (_,true) as hd::tl ->
        (* Already used quantifier => keep it *)
        hd::iter tl
      | ((_,v,_) as q,false) as hd::tl ->
        (* Check if this unused quantifier is present in the bounds of an
           already used quantifier *)
        if List.exists (function
            | ((_,_,S_interval(lo,hi)),true) ->
              var_in_expr v lo || var_in_expr v hi
            | _ -> false
          ) tl
        then (q,true)::iter tl
        else hd::iter tl
    in
    (* Iterate [iter] until no new used quantifier is found *)
    let rec fp quants =
      let quants' = iter quants in
      if List.exists (fun ((_,b1),(_,b2)) -> b1 != b2) (List.combine quants quants') then
        fp quants'
      else
        quants
    in
    fp quants |>
    (* Keep only used quantifiers *)
    List.filter (fun (_,b) -> b) |>
    List.map fst


  (** Translate a prenex encoding (i.e. quantifiers and a condition) into an expression *)
  let rec prenex_to_expr quants cond range =
    if quants = [] then cond
    else
      match ekind cond with
      | E_binop(O_log_and, e1, e2) ->
        let quants1 = remove_unnecessary_quantifiers quants e1 in
        let quants2 = remove_unnecessary_quantifiers quants e2 in
        let e1' = prenex_to_expr quants1 e1 e1.erange in
        let e2' = prenex_to_expr quants2 e2 e2.erange in
        mk_log_and e1' e2' range

      | E_binop(O_log_or, e1, e2) ->
        let quants1 = remove_unnecessary_quantifiers quants e1 in
        let quants2 = remove_unnecessary_quantifiers quants e2 in
        let e1' = prenex_to_expr quants1 e1 e1.erange in
        let e2' = prenex_to_expr quants2 e2 e2.erange in
        mk_log_or e1' e2' range

      | E_stub_if(c,e1,e2) ->
        let quants' = remove_unnecessary_quantifiers quants c in
        if quants' = [] then
          let e1' = prenex_to_expr quants e1 e1.erange in
          let e2' = prenex_to_expr quants e2 e2.erange in
          { cond with ekind = E_stub_if(c, e1', e2') }
        else
          mk_stub_quantified_formula quants cond range

      | E_stub_otherwise(e,a) ->
        mk_stub_otherwise (prenex_to_expr quants e e.erange) a range

      | _ ->
        let quants' = remove_unnecessary_quantifiers quants cond in
        if quants' = [] then
          cond
        else
          mk_stub_quantified_formula quants' cond range

  (** Evaluate a formula *)
  let eval_formula
      (cond_to_stmt: expr -> range -> stmt)
      (f: formula with_range)
      man flow =
    (* Write formula in prenex normal form *)
    let quants,cond = formula_to_prenex f in
    (* Translate the prenex encoding into an expression *)
    let cond' = prenex_to_expr quants cond f.range in
    (* Constrain the environment with the obtained condition *)
    man.exec (cond_to_stmt cond' f.range) flow


  (** Initialize the parameters of the stubbed function *)
  let rec init_params args params range man flow =
    match params, args with
    | [], _ -> Post.return flow
    | param::tl_params, arg::tl_args ->
      man.exec (mk_add_var param range) flow >>%
      man.exec (mk_assign (mk_var param range) arg range) >>%
      init_params tl_args tl_params range man
    | _, [] ->
      panic "stubs: insufficent number of arguments"


  (** Remove parameters from the returned flow *)
  let remove_params params range man flow =
    man.exec (mk_block (List.map (fun param -> mk_remove_var param range) params) range) flow 

  (** Evaluate the formula of the `assumes` section *)
  let exec_assumes assumes man flow =
    eval_formula mk_assume assumes.content man flow


  (** Evaluate the formula of the `requires` section *)
  let exec_requires req man flow =
    eval_formula mk_stub_requires req.content man flow


  (** Execute an allocation of a new resource *)
  let exec_local_new v res range man flow : 'a post =
    (* Evaluation the allocation request *)
    man.eval (mk_stub_alloc_resource res range) flow >>$ fun addr flow ->
    (* Assign the address to the variable *)
      man.exec (mk_assign (mk_var v range) addr range) flow


  (** Execute a function call *)
  (* FIXME: check the purity of f *)
  let exec_local_call v f args range man flow =
    man.exec (mk_assign
                (mk_var v range)
                (mk_expr (E_call(f, args)) ~etyp:v.vtyp range)
                range
             ) flow


  (** Execute the `local` section *)
  let exec_local l man flow =
    match l.content.lval with
    | L_new  res -> exec_local_new l.content.lvar res l.range man flow
    | L_call (f, args) -> exec_local_call l.content.lvar f args l.range man flow


  let exec_ensures e return man flow =
    (* Replace E_stub_return expression with the fresh return variable *)
    let f =
      match return with
      | None -> e.content
      | Some v ->
        visit_expr_in_formula
          (fun e ->
             match ekind e with
             | E_stub_return -> Keep { e with ekind = E_var (v, None) }
             | _ -> VisitParts e
          )
          e.content
    in
    (* Evaluate ensure body and return flows that verify it *)
    eval_formula mk_assume f man flow


  let exec_assigns assigns man flow =
    let stmt = mk_stub_assigns assigns.content.assign_target assigns.content.assign_offset assigns.range in
    match assigns.content.assign_offset with
    | [] ->
      man.exec stmt flow

    | (l,u)::tl ->
      (* Check that offsets intervals are not empty *)
      let range = tag_range assigns.range "condition" in
      let cond = List.fold_left
          (fun acc (l,u) -> log_and acc (le l u range) assigns.range)
          (le l u range) tl
      in
      assume cond
        ~fthen:(fun flow -> man.exec stmt flow)
        ~felse:(fun flow -> Post.return flow)
        man flow



  (** Remove locals *)
  let clean_post locals range man flow =
    let block =
      List.fold_left (fun block l ->
          mk_remove_var l.content.lvar range :: block
        ) [] locals
    in
    man.exec (mk_block block range) flow


  let exec_free free man flow =
    let e = free.content in
    let stmt = mk_stub_free e free.range in
    man.exec stmt flow


  let exec_message msg man flow =
    if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
    then Post.return flow
    else match msg.content.message_kind with
      | WARN ->
        Exceptions.warn_at msg.range "%s" msg.content.message_body;
        Post.return flow

      | UNSOUND ->
        Post.return @@ Flow.add_local_assumption (Soundness.A_stub_soundness_message msg.content.message_body) msg.range flow


  (** Execute a leaf section *)
  let exec_leaf leaf return man flow : 'a post =
    match leaf with
    | S_local local -> exec_local local man flow 
    | S_assumes assumes -> exec_assumes assumes man flow
    | S_requires requires -> exec_requires requires man flow
    | S_assigns assigns -> exec_assigns assigns man flow
    | S_ensures ensures -> exec_ensures ensures return man flow
    | S_free free -> exec_free free man flow
    | S_message msg -> exec_message msg man flow

  (** Execute the body of a case section *)
  let exec_case ?(stub=None) case return man flow : 'a post =
    let post =
      match stub with
      | None -> Post.return flow
      | Some stub ->
        man.exec (mk_add_marker (M_stub_case(stub, case)) case.case_range) flow
    in
    let post = List.fold_left (fun acc leaf ->
        acc >>% fun flow -> exec_leaf leaf return man flow
      ) post case.case_body in
    post >>%
    (* Clean case post state *)
    clean_post case.case_locals case.case_range man


  (** Execute the body of a stub *)
  let exec_body ?(stub=None) body return range man (flow : 'a flow) =
    (* Execute leaf sections *)
    let post = List.fold_left (fun post section ->
        match section with
        | S_leaf leaf -> post >>% fun flow -> exec_leaf leaf return man flow 
        | _ -> post
      ) (Post.return flow) body
    in
    post >>% fun flow ->
    (* Execute case sections separately *)
    let flows, ctx = List.fold_left (fun (acc,ctx) section ->
        match section with
        | S_case case when not (is_case_ignored stub case) ->
          let flow = Flow.set_ctx ctx flow in
          let flow' = exec_case ~stub case return man flow in
          flow':: acc, Cases.get_ctx flow'
        | _ -> acc, ctx
      ) ([], Flow.get_ctx flow) body
    in
    let flows = List.map (Cases.set_ctx ctx) flows in
    (* Join flows *)
    (* FIXME: when the cases do not define a partitioning, we need
         to do something else *)
    Cases.join_list flows ~empty:(fun () -> Post.return flow)


  let prepare_all_assigns assigns range man flow =
    (* Check if there are assigned variables *)
    if assigns = []
    then Post.return flow
    else man.exec (mk_stub_prepare_all_assigns assigns range) flow

  let clean_all_assigns assigns range man flow =
    (* Check if there are assigned variables *)
    if assigns = []
    then Post.return flow
    else man.exec (mk_stub_clean_all_assigns assigns range) flow

  (** Evaluate a call to a stub *)
  let eval_stub_call stub args return range man flow =
      (* Update the callstack *)
      let cs = Flow.get_callstack flow in
      let flow = Flow.push_callstack stub.stub_func_name range flow in
      (* Initialize parameters *)
      init_params args stub.stub_func_params range man flow >>% fun flow ->
      (* Prepare assignments *)
      prepare_all_assigns stub.stub_func_assigns (tag_range stub.stub_func_range "prepare") man flow >>% fun flow ->
      (* Create the return variable *)
      (match return with
        | None -> Post.return flow
        | Some v -> man.exec (mk_add_var v range) flow) >>% fun flow ->
      (* Evaluate the body of the stb *)
      exec_body ~stub:(Some stub) stub.stub_func_body return range man flow >>% fun flow ->
      (* Clean locals *)
      clean_post stub.stub_func_locals (tag_range stub.stub_func_range "clean") man flow >>% fun flow ->
      (* Clean assignments *)
      clean_all_assigns stub.stub_func_assigns (tag_range stub.stub_func_range "clean") man flow >>% fun flow ->
      (* Restore the callstack *)
      let flow = Flow.set_callstack cs flow in
      let clean_range = tag_range range "clean" in
      let cleaners = List.map (fun param -> mk_remove_var param clean_range) stub.stub_func_params in

      match return with
      | None ->
        Eval.singleton (mk_unit range) flow ~cleaners

      | Some v ->
        man.eval (mk_var v range) flow |>
        Cases.add_cleaners (mk_remove_var v range :: cleaners)

  (** Evaluate an otherwise expression *)
  let eval_otherwise cond alarm range man flow =
    assume cond man flow
      ~fthen:(fun flow -> safe_stub_condition cond.erange man flow |>
                          Eval.singleton (mk_true range))
      ~felse:(fun flow ->
          match alarm with
          | Some e -> man.eval e flow
          | None   -> raise_stub_invalid_requirement ~bottom:false cond range man flow |>
                      Eval.singleton (mk_false range)
        )

  (* Remove flows where a quantification interval is empty *)
  let discard_empty_quantification_intervals quants cond range man flow =
    let remove_quant_vars vars evl =
      if vars = [] then evl
      else
        evl >>$ fun e flow ->
        List.fold_left (fun acc v ->
            acc >>% man.exec (mk_remove_var v range)
          ) (Post.return flow) vars
        >>% fun flow ->
        Eval.singleton e flow
    in
    let rec iter added l flow =
      match l with
      | [] ->
        man.eval ~route:(Below name) (mk_stub_quantified_formula quants cond range) flow |>
        remove_quant_vars added

      | (_,_,S_resource _)::tl ->
        iter added tl flow

      | (FORALL,v,S_interval(lo,hi))::tl ->
        assume
          (mk_le lo hi range) man flow
          ~fthen:(fun flow ->
              man.exec (mk_add_var v range) flow >>%
              man.exec (mk_assume (mk_in (mk_var v range) lo hi range) range) >>%
              iter (v::added) tl
            )
          ~felse:(fun flow ->
              Eval.singleton (mk_true range) flow |>
              remove_quant_vars added
            )

      | (EXISTS,v,S_interval(lo,hi))::tl ->
        assume
          (mk_le lo hi range) man flow
          ~fthen:(fun flow ->
              man.exec (mk_add_var v range) flow >>%
              man.exec (mk_assume (mk_in (mk_var v range) lo hi range) range) >>%
              iter (v::added) tl
            )
          ~felse:(fun flow ->
              Eval.singleton (mk_false range) flow |>
              remove_quant_vars added
            )
    in
    iter [] quants flow


  (** Check if a condition contains an otherwise expression *)
  let rec otherwise_in_condition cond =
    match ekind cond with
    | E_stub_otherwise _ -> true
    | E_binop((O_log_and | O_log_or), cond1, cond2) ->
      otherwise_in_condition cond1 || otherwise_in_condition cond2
    | E_stub_quantified_formula(_, qcond) ->
      otherwise_in_condition qcond
    | E_stub_if(_,fthen,felse) ->
      otherwise_in_condition fthen || otherwise_in_condition felse
    | _ -> false

  (** Remove newly introduced checks *)
  let remove_new_checks old flow =
    let report =
      fold2zo_report
        (fun diag1 acc -> acc)
        (fun diag2 acc -> remove_diagnostic diag2 acc )
        (fun diag1 diag2 acc -> acc)
        (Flow.get_report old) (Flow.get_report flow)
        (Flow.get_report flow)
    in
    Flow.set_report report flow

  (** Move newly introduced checks to a new range *)
  let move_new_checks range old flow =
    let report =
      fold2zo_report
        (fun diag1 acc -> acc)
        (fun diag2 acc -> remove_diagnostic diag2 acc |>
                          add_diagnostic {diag2 with diag_range = range} )
        (fun diag1 diag2 acc -> acc)
        (Flow.get_report old) (Flow.get_report flow)
        (Flow.get_report flow)
    in
    Flow.set_report report flow

  (** Entry point of expression evaluations *)
  let eval exp man flow =
    match ekind exp with
    | E_stub_call (stub, args) ->
      if man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then Cases.empty flow |> OptionExt.return 
      else 
      (* Create the return variable *)
      let return =
        match stub.stub_func_return_type with
        | None   -> None
        | Some t -> Some (Universal.Iterators.Interproc.Common.mk_return exp)
      in
      eval_stub_call stub args return exp.erange man flow |>
      OptionExt.return

    | E_stub_otherwise(cond, alarm) ->
      eval_otherwise cond alarm exp.erange man flow |>
      OptionExt.return

    | E_stub_raise msg ->
      raise_stub_alarm ~bottom:false msg exp.erange man flow |>
      Eval.singleton (mk_false exp.erange) |>
      OptionExt.return

    | E_binop(O_log_and, e1, e2) when otherwise_in_condition e1 && otherwise_in_condition e2 ->
      (* To evaluate a requirement e1 ∧ e2, we evaluate e1 and e2 and then we
         lift checks on e1 and e2 to e1 ∧ e2:
         - If both e1 and e2 are valid, then we mark e1 ∧ e2 as safe.
           We also remove checks on e1 and e2, since they are redundant with
           the check on e1 ∧ e2.
         - If e1 or e2 is invalid, we move the alarms to the range of e1 ∧ e2, but we keep the same alarm message.
      *)
      let flow0 = flow in
      assume e1 man flow
        ~fthen:(fun flow ->
            assume e2 man flow
              ~fthen:(fun flow ->
                  remove_new_checks flow0 flow |>
                  safe_stub_condition exp.erange man |>
                  Eval.singleton (mk_true exp.erange)
                )
              ~felse:(fun flow ->
                  move_new_checks exp.erange flow0 flow |>
                  Eval.singleton (mk_false exp.erange)
                )
          )
        ~felse:(fun flow ->
            move_new_checks exp.erange flow0 flow |>
            Eval.singleton (mk_false exp.erange)
          ) |>
      OptionExt.return

    | E_binop(O_log_or, e1, e2) when otherwise_in_condition e1 && otherwise_in_condition e2 ->
      (* To evaluate a requirement e1 ∨ e2, we evaluate e1 and e2 and then we
         lift checks on e1 and e2 to e1 ∨ e2:
         - If e1 or e2 is valid, then we mark e1 ∨ e2 as safe.
           We also remove checks on e1 and e2, since they are redundant with
           the check on e1 ∨ e2.
         - If e1 and e2 are invalid, we move the alarms to the range of e1 ∨ e2, but we keep the same alarm message.
      *)
      let flow0 = flow in
      assume e1 man flow
        ~fthen:(fun flow  ->
            remove_new_checks flow0 flow |>
            safe_stub_condition exp.erange man |>
            Eval.singleton (mk_true exp.erange)
          )
        ~felse:(fun flow ->
            assume e2 man flow
              ~fthen:(fun flow ->
                  remove_new_checks flow0 flow |>
                  safe_stub_condition exp.erange man |>
                  Eval.singleton (mk_true exp.erange)
                )
              ~felse:(fun flow ->
                  move_new_checks exp.erange flow0 flow |>
                  Eval.singleton (mk_false exp.erange)
                )
          ) |>
      OptionExt.return

    | E_stub_quantified_formula(quants, cond)
      when List.exists (function (_,_,S_interval _) -> true | _ -> false) quants ->
      discard_empty_quantification_intervals quants cond exp.erange man flow |>
      OptionExt.return

    | E_stub_if(c,f1,f2) ->
      assume c man flow
        ~fthen:(man.eval f1)
        ~felse:(man.eval f2) |>
      OptionExt.return

    | _ -> None


  (** Computation of post-conditions *)
  (** ============================== *)

  (** Execute a global stub directive *)
  let exec_directive stub range man flow =
      (* Prepare assignments *)
      prepare_all_assigns stub.stub_directive_assigns stub.stub_directive_range man flow >>% fun flow ->
      (* Evaluate the body of the stub *)
      exec_body stub.stub_directive_body None range man flow >>% fun flow ->
      (* Clean locals *)
      clean_post stub.stub_directive_locals stub.stub_directive_range man flow >>% fun flow ->
      (* Clean assignments *)
      clean_all_assigns stub.stub_directive_assigns stub.stub_directive_range man flow


  (** Normalize a requirement condition by adding missing otherwise decorations *)
  let rec normalize_requirement_condition cond =
    match ekind cond with
    | E_stub_otherwise _ ->
      cond

    | E_binop(O_log_and, e1, e2) ->
      let e1' = normalize_requirement_condition e1 in
      let e2' = normalize_requirement_condition e2 in
      mk_log_and e1' e2' cond.erange

    | E_binop(O_log_or, e1, e2) ->
      let e1' = normalize_requirement_condition e1 in
      let e2' = normalize_requirement_condition e2 in
      mk_log_or e1' e2' cond.erange


    | E_stub_quantified_formula(quants, {ekind = E_stub_otherwise(qcond, alarm)}) ->
      mk_stub_otherwise (mk_stub_quantified_formula quants qcond cond.erange) alarm cond.erange

    | E_stub_if(c,e1,e2) ->
      let e1' = normalize_requirement_condition e1 in
      let e2' = normalize_requirement_condition e2 in
      { cond with ekind = E_stub_if(c,e1',e2') }

    | _ ->
      mk_stub_otherwise cond None cond.erange


  (** Check a stub requirement *)
  let exec_requires cond range man flow =
    (* Normalize the condition so that all sub-conditions are decorated with an
       adequate `otherwise` expression *)
    let cond' = normalize_requirement_condition cond in
    (* Evaluate the condition. Note that the evaluation of otherwise expression
       is responsible for raising the alarm if a condition is not satisified. *)
    man.eval cond' flow >>$ fun r flow ->
    match ekind r with
    | E_constant (C_bool true)  -> Post.return flow
    | E_constant (C_bool false) -> Flow.remove T_cur flow |>
                                   Post.return
    | _ -> assert false


  let exec stmt man flow =
    match skind stmt with
    | S_stub_directive (stub) ->
      exec_directive stub stmt.srange man flow |>
      OptionExt.return

    | S_stub_requires cond ->
      exec_requires cond stmt.srange man flow |>
      OptionExt.return

    | _ -> None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None


  (** Pretty printer *)
  (** ============== *)

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
