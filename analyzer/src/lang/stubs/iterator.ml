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
open Zone
open Alarms



module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "stubs.iterator"
    end)

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = [Z_stubs]; uses = []};
    ieval = {provides = [Z_stubs, Z_any]; uses = []};
  }

  let alarms = []


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = flow


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
          flip_quantified_var var set ff |>
          negate_formula
        )) f.range

    | F_exists (var, set, ff) ->
      with_range (F_forall (
          var,
          set,
          flip_quantified_var var set ff |>
          negate_formula
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


  let combine_alarms op flow1 flow2 flow range =
    let alarms1 = Flow.get_alarms flow1 in
    let alarms2 = Flow.get_alarms flow2 in
    let alarms = Flow.get_alarms flow in
    let diff1 = AlarmSet.diff alarms1 alarms in
    let diff2 = AlarmSet.diff alarms2 alarms in
    let is_requires_alarm a =
      match get_alarm_class a with
      | A_stub_invalid_requires -> true
      | _ -> false in
    let get_requires_condition a =
      match get_alarm_message a with
      | A_stub_invalid_requires_condition cond -> cond
      | _ -> assert false in
    let combine_requires_alarms op =
      let requires1,diff1' = AlarmSet.partition is_requires_alarm diff1 in
      let requires2,diff2' = AlarmSet.partition is_requires_alarm diff2 in
      let f = match op with
        | AND -> mk_log_and
        | OR -> mk_log_or
        | IMPLIES -> assert false in
      if AlarmSet.is_empty requires1 || AlarmSet.is_empty requires2 then
        AlarmSet.union diff1 diff2
      else
        AlarmSet.fold
          (fun a1 acc ->
             let cond1 = get_requires_condition a1 in
             let a = AlarmSet.map (fun a2 ->
                 let cond2 = get_requires_condition a2 in
                 mk_alarm (A_stub_invalid_requires_condition (f cond1 cond2 range)) (get_alarm_callstack a1) (get_alarm_range a1)
               ) requires2 in
             AlarmSet.union a acc
          ) requires1 (AlarmSet.union diff1' diff2') in
    match op with
    | AND ->
      if AlarmSet.is_empty diff1 && AlarmSet.is_empty diff2 then
        alarms
      else
        AlarmSet.union (combine_requires_alarms AND) alarms

    | OR ->
      if AlarmSet.is_empty diff1 || AlarmSet.is_empty diff2 then
        alarms
      else
        AlarmSet.union (combine_requires_alarms OR) alarms

    | IMPLIES ->
      assert false


    (*   AlarmSet.fold2_diff
     * match AlarmSet.subset (Flow.get_alarms f1) (Flow.get_alarms flow),
     *       AlarmSet.subset (Flow.get_alarms f2) (Flow.get_alarms flow)
     * with
     * | false,true
     * | true,false ->
     *   (\* Only one evaluation detected alarms, so ignore them *\)
     *   Flow.get_alarms flow
     *
     * | true, true
     * | false, false ->
     *   (\* Both evaluations behave similarly, so keep them *\)
     *   AlarmSet.union (Flow.get_alarms f1) (Flow.get_alarms f2) *)



  let rec eval_formula
      (cond_to_stmt: expr -> range -> stmt)
      (f: formula with_range)
      range
      (man:('a, unit, 's) man)
      (flow:'a flow)
    : 'a flow =
    debug "@[<hov>eval formula@ %a@]" pp_formula f;
    match f.content with
    | F_expr e ->
      man.exec (cond_to_stmt e range) flow

    | F_binop (AND, f1, f2) ->
      let flow1 = eval_formula cond_to_stmt f1 range man flow in
      let flow2 = eval_formula cond_to_stmt f2 range man (Flow.set_alarms (Flow.get_alarms flow) flow1) in
      let alarms = combine_alarms AND flow1 flow2 flow range in
      Flow.set_alarms alarms flow2

    | F_binop (OR, f1, f2) ->
      let flow1 = eval_formula cond_to_stmt f1 range man flow in
      let flow2 = eval_formula cond_to_stmt f2 range man flow in
      let alarms = combine_alarms OR flow1 flow2 flow range in
      Flow.join man.lattice flow1 flow2 |>
      Flow.set_alarms alarms


    | F_binop (IMPLIES, f1, f2) ->
      let nf1 = eval_formula mk_assume (negate_formula f1) range man flow in
      let f2 = eval_formula mk_assume f1 range man flow |>
               eval_formula cond_to_stmt f2 range man
      in
      Flow.join man.lattice nf1 f2

    | F_not ff ->
      let ff' = negate_formula ff in
      eval_formula cond_to_stmt ff' range man flow

    | F_forall (v, s, ff) ->
      eval_quantified_formula cond_to_stmt FORALL v s ff range man flow

    | F_exists (v, s, ff) ->
      eval_quantified_formula cond_to_stmt EXISTS v s ff range man flow

    | F_in (e, S_interval (l, u)) ->
      man.exec (cond_to_stmt (mk_in e l u f.range) range) flow

    | F_in (e, S_resource res ) ->
      man.exec (cond_to_stmt (mk_stub_resource_mem e res f.range) range) flow


  (** Evaluate a quantified formula and its eventual negation *)
  and eval_quantified_formula cond_to_stmt q v s f range man flow : 'a flow =
    (* Check that the set [s] is not empty *)
    let cond =
      match s with
      | S_resource _ -> mk_true range
      | S_interval(a,b) -> mk_binop a O_le b range
    in
    assume_flow cond
      ~fthen:(fun flow ->
          (* Add [v] to the environment *)
          let flow = man.exec (mk_add_var v range) flow in
          (* Ensure that [v] is in the set [s] *)
          let flow = match s with
            | S_resource _ -> flow
            | S_interval(a,b) -> man.exec (mk_assume (mk_in (mk_var v range) a b range) range) flow
          in
          (* Replace [v] in [f] with quantified variables *)
          let f' = visit_expr_in_formula
              (fun e ->
                 match ekind e with
                 | E_var (vv, _) when compare_var v vv = 0 ->
                   Keep (mk_stub_quantified q v s range)

                 | _ -> VisitParts e
              )
              f
          in
          (* Evaluate [f'] *)
          let flow = eval_formula cond_to_stmt f' range man flow in
          (* Remove [v] from the environment *)
          man.exec (mk_remove_var v range) flow
        )
      ~felse:(fun flow ->
          match q with
          | FORALL -> man.exec (cond_to_stmt (mk_true range) range) flow
          | EXISTS -> man.exec (cond_to_stmt (mk_false range) range) flow
        ) man flow




  (** Initialize the parameters of the stubbed function *)
  let init_params args params range man flow =
    List.combine args params |>
    List.fold_left (fun flow (arg, param) ->
        man.exec (mk_assign (mk_var param range) arg range) flow
      ) flow

  (** Remove parameters from the returned flow *)
  let remove_params params range man flow =
    params |> List.fold_left (fun flow param ->
        man.exec (mk_remove_var param range) flow
      ) flow


  (** Evaluate the formula of the `assumes` section *)
  let exec_assumes assumes range man flow =
    eval_formula mk_assume assumes.content range man flow


  (** Evaluate the formula of the `requires` section *)
  let exec_requires req range man flow =
    eval_formula mk_stub_requires req.content range man flow


  (** Execute an allocation of a new resource *)
  let exec_local_new v res alloc_range call_range man flow : 'a flow =
    (* Evaluation the allocation request *)
    post_to_flow man (
      man.eval (mk_stub_alloc_resource res alloc_range) flow >>$ fun addr flow ->
      (* Assign the address to the variable *)
      man.post (mk_assign (mk_var v call_range) addr alloc_range) flow
    )


  (** Execute a function call *)
  (* FIXME: check the purity of f *)
  let exec_local_call v f args local_range call_range man flow =
    man.exec (mk_assign
                (mk_var v local_range)
                (mk_expr (E_call(f, args)) ~etyp:v.vtyp local_range)
                local_range
             ) flow


  (** Execute the `local` section *)
  let exec_local l range man flow =
    match l.content.lval with
    | L_new  res -> exec_local_new l.content.lvar res l.range range man flow
    | L_call (f, args) -> exec_local_call l.content.lvar f args l.range range man flow


  let exec_ensures e return range man flow =
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
    eval_formula mk_assume f range man flow


  let exec_assigns assigns range man flow =
    let stmt = mk_stub_assigns assigns.content.assign_target assigns.content.assign_offset range in
    match assigns.content.assign_offset with
    | [] ->
      man.exec stmt flow

    | (l,u)::tl ->
      (* Check that offsets intervals are not empty *)
      let cond = List.fold_left
          (fun acc (l,u) -> log_and acc (le l u assigns.range) assigns.range)
          (le l u assigns.range) tl
      in
      assume_flow cond
        ~fthen:(fun flow -> man.exec stmt flow)
        ~felse:(fun flow -> flow)
        man flow



  (** Remove locals *)
  let clean_post locals range man flow =
    let block =
      List.fold_left (fun block l ->
          mk_remove_var l.content.lvar range :: block
        ) [] locals
    in
    man.exec (mk_block block range) flow


  let exec_free free range man flow =
    let e = free.content in
    let stmt = mk_stub_free e range in
    man.exec stmt flow


  let exec_message msg range man flow =
    if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
    then flow
    else match msg.content.message_kind with
      | WARN ->
        Exceptions.warn_at range "%s" msg.content.message_body;
        flow

      | UNSOUND ->
        Soundness.warn_at range "%s" msg.content.message_body;
        flow

      | ALARM ->
        raise_stub_alarm msg.content.message_body range man flow



  (** Execute a leaf section *)
  let exec_leaf leaf return range man flow =
    match leaf with
    | S_local local -> exec_local local range man flow
    | S_assumes assumes -> exec_assumes assumes range man flow
    | S_requires requires -> exec_requires requires range man flow
    | S_assigns assigns -> exec_assigns assigns range man flow
    | S_ensures ensures -> exec_ensures ensures return range man flow
    | S_free free -> exec_free free range man flow
    | S_message msg -> exec_message msg range man flow

  (** Execute the body of a case section *)
  let exec_case case return range man flow =
    List.fold_left (fun acc leaf ->
        exec_leaf leaf return range man acc
      ) flow case.case_body |>

    (* Clean case post state *)
    clean_post case.case_locals case.case_range man


  (** Execute the body of a stub *)
  let exec_body ?(stub=None) body return range man flow =
    (* Execute leaf sections *)
    let flow = List.fold_left (fun flow section ->
        match section with
        | S_leaf leaf -> exec_leaf leaf return range man flow
        | _ -> flow
      ) flow body
    in

    (* Execute case sections separately *)
    let flows, ctx = List.fold_left (fun (acc,ctx) section ->
        match section with
        | S_case case when not (is_case_ignored stub case) ->
          let flow = Flow.set_ctx ctx flow in
          let flow' = exec_case case return range man flow in
          flow':: acc, Flow.get_ctx flow'
        | _ -> acc, ctx
      ) ([], Flow.get_ctx flow) body
    in

    let flows = List.map (Flow.set_ctx ctx) flows in

    (* Join flows *)
    (* FIXME: when the cases do not define a partitioning, we need
         to do something else *)
    Flow.join_list man.lattice flows ~empty:(fun () -> flow)


  let prepare_all_assigns assigns range man flow =
    (* Check if there are assigned variables *)
    if assigns = []
    then flow
    else man.exec (mk_stub_prepare_all_assigns assigns range) flow

  let clean_all_assigns assigns range man flow =
    (* Check if there are assigned variables *)
    if assigns = []
    then flow
    else man.exec (mk_stub_clean_all_assigns assigns range) flow

  (** The following patch_params_* functions are used to patch the body of a stub by
      adding call arguments to the evaluation history of formal
      parameters *)
  let patch_params_history_visitor bindings exp =
    match ekind exp with
    | E_var(v,_) ->
      begin
        match List.find_opt
                (fun (p,a) -> compare_var p v = 0) bindings with
        | None -> Visitor.Keep exp
        | Some (p,a) -> Keep { exp with eprev = Some a }
      end
    | _ -> VisitParts exp

  let patch_params_history_in_expr bindings exp =
    Visitor.map_expr
      (patch_params_history_visitor bindings)
      (fun s -> VisitParts s)
      exp

  let patch_params_history_in_local_value bindings = function
    | L_new _ as x -> x
    | L_call(f,cargs) -> L_call (patch_params_history_in_expr bindings f,
                                 List.map (patch_params_history_in_expr bindings) cargs)

  let patch_params_history_in_local bindings local =
    bind_range local @@ fun l ->
    { l with
      lval = patch_params_history_in_local_value bindings l.lval }

  let patch_params_history_in_formula bindings f =
    visit_expr_in_formula (patch_params_history_visitor bindings) f

  let patch_params_history_in_assumes bindings assumes =
    bind_range assumes @@ (patch_params_history_in_formula bindings)

  let patch_params_history_in_requires bindings requires =
    bind_range requires @@ (patch_params_history_in_formula bindings)

  let patch_params_history_in_ensures bindings ensures =
    bind_range ensures @@ (patch_params_history_in_formula bindings)

  let patch_params_history_in_interval bindings (lo,hi) =
    ( patch_params_history_in_expr bindings lo,
      patch_params_history_in_expr bindings hi )

  let patch_params_history_in_assigns bindings assigns =
    bind_range assigns @@ fun a ->
    { assign_target = patch_params_history_in_expr bindings a.assign_target;
      assign_offset = List.map (patch_params_history_in_interval bindings) a.assign_offset; }

  let patch_params_history_in_free bindings free =
    bind_range free @@ (patch_params_history_in_expr bindings)

  let patch_params_history_in_leaf bindings = function
    | S_local local -> S_local (patch_params_history_in_local bindings local)
    | S_assumes assumes -> S_assumes (patch_params_history_in_assumes bindings assumes)
    | S_requires requires -> S_requires (patch_params_history_in_requires bindings requires)
    | S_assigns assigns -> S_assigns (patch_params_history_in_assigns bindings assigns)
    | S_ensures ensures -> S_ensures (patch_params_history_in_ensures bindings ensures)
    | S_free free -> S_free (patch_params_history_in_free bindings free)
    | S_message _ as x -> x

  let patch_params_history_in_case bindings case =
    { case with
      case_body = List.map (patch_params_history_in_leaf bindings) case.case_body;
      case_locals = List.map (patch_params_history_in_local bindings) case.case_locals;
      case_assigns = List.map (patch_params_history_in_assigns bindings) case.case_assigns; }

  let patch_params_history_in_section bindings = function
    | S_case case -> S_case (patch_params_history_in_case bindings case)
    | S_leaf leaf -> S_leaf (patch_params_history_in_leaf bindings leaf)

  let patch_params_history_in_stub bindings (stub:stub_func) : stub_func =
    { stub with
      stub_func_body = List.map (patch_params_history_in_section bindings) stub.stub_func_body;
      stub_func_locals = List.map (patch_params_history_in_local bindings) stub.stub_func_locals;
      stub_func_assigns = List.map (patch_params_history_in_assigns bindings) stub.stub_func_assigns; }

  (** Entry point of expression evaluations *)
  let eval zone exp man flow =
    match ekind exp with
    | E_stub_call (stub, args) ->
      debug "call to stub %s:@\n @[%a@]"
        stub.stub_func_name
        pp_stub_func stub
      ;

      (* Update the callstack *)
      let cs = Flow.get_callstack flow in
      let flow = Flow.push_callstack stub.stub_func_name exp.erange flow in

      (* In order to get better alarm messages, we replace parameters
         with the corresponding argument. This is done by putting the
         argument in the evaluation history of the parameter. Function
         `get_orig_expr` can be used in order to recover the original
         form. *)
      let bindings = List.combine stub.stub_func_params args in
      let stub = patch_params_history_in_stub bindings stub in

      (* Initialize parameters *)
      let flow = init_params args stub.stub_func_params exp.erange man flow in

      (* Prepare assignments *)
      let flow = prepare_all_assigns stub.stub_func_assigns stub.stub_func_range man flow in

      (* Create the return variable *)
      let return, flow =
        match stub.stub_func_return_type with
        | None -> None, flow
        | Some t ->
          let return = Universal.Iterators.Interproc.Common.mk_return_var exp in
          let flow = man.exec (mk_add_var return exp.erange) flow in
          Some return, flow
      in

      (* Evaluate the body of the stb *)
      let flow = exec_body ~stub:(Some stub) stub.stub_func_body return exp.erange man flow in

      (* Clean locals *)
      let flow = clean_post stub.stub_func_locals stub.stub_func_range man flow in

      (* Clean assignments *)
      let flow = clean_all_assigns stub.stub_func_assigns stub.stub_func_range man flow in

      (* Restore the callstack *)
      let flow = Flow.set_callstack cs flow in

      let cleaners = List.map (fun param -> mk_remove_var param exp.erange) stub.stub_func_params in

      begin match return with
        | None ->
          Eval.singleton (mk_unit exp.erange) flow ~cleaners |>
          OptionExt.return

        | Some v ->
          Eval.singleton (mk_var v exp.erange) flow ~cleaners:(mk_remove_var v exp.erange :: cleaners) |>
          OptionExt.return
      end

    | _ -> None


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_stub_directive (stub) ->
      (* Prepare assignments *)
      let flow = prepare_all_assigns stub.stub_directive_assigns stub.stub_directive_range man flow in

      (* Evaluate the body of the stub *)
      let flow = exec_body stub.stub_directive_body None stmt.srange man flow in

      (* Clean locals *)
      let flow = clean_post stub.stub_directive_locals stub.stub_directive_range man flow in

      (* Clean assignments *)
      let flow = clean_all_assigns stub.stub_directive_assigns stub.stub_directive_range man flow in

      Post.return flow |>
      OptionExt.return

    | _ -> None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  register_stateless_domain (module Domain)
