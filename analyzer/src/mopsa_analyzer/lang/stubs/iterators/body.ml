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

(** FIXME: remove calls to post_to_flow to preserve logs *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Alarms


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "stubs.iterators.body"
    end)

  let checks = [CHK_STUB_ALARM]


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


  (** Translate a formula into prenex normal form *)
  let rec to_prenex_formula f man flow =
    match f.content with
    | F_expr cond ->
      Cases.singleton ([], cond) flow

    | F_binop (AND, f1, f2) ->
      to_prenex_formula f1 man flow >>$ fun (quants1, cond1) flow ->
      to_prenex_formula f2 man flow >>$ fun (quants2, cond2) flow ->
      Cases.singleton (quants1@quants2, mk_log_and cond1 cond2 f.range) flow

    | F_binop (OR, f1, f2) ->
      to_prenex_formula f1 man flow >>$ fun (quants1, cond1) flow ->
      to_prenex_formula f2 man flow >>$ fun (quants2, cond2) flow ->
      Cases.singleton (quants1@quants2, mk_log_or cond1 cond2 f.range) flow

    | F_binop (IMPLIES, f1, f2) ->
      to_prenex_formula (negate_formula f1) man flow >>$ fun (quants1, cond1) flow ->
      to_prenex_formula f2 man flow >>$ fun (quants2, cond2) flow ->
      Cases.singleton (quants1@quants2, mk_log_or cond1 cond2 f.range) flow

    | F_not ff ->
      to_prenex_formula (negate_formula ff) man flow

    | F_in (e, S_interval (l, u)) ->
      Cases.singleton ([], mk_in e l u f.range) flow

    | F_in (e, S_resource res ) ->
      Cases.singleton ([], mk_stub_resource_mem e res f.range) flow

    | F_forall(v,(S_resource _ as s),ff) ->
      to_prenex_formula ff man flow >>$ fun (quants,cond) flow ->
      Cases.singleton ((FORALL,v,s)::quants, cond) flow

    | F_forall(v,(S_interval (lo,hi) as s),ff) ->
      assume
        (mk_le lo hi f.range)
        ~fthen:(fun flow ->
            man.exec (mk_add_var v f.range) flow >>%
            man.exec (mk_assume (mk_in (mk_var v f.range) lo hi f.range) f.range) >>%
            to_prenex_formula ff man >>$ fun (quants,cond) flow ->
            Cases.singleton ((FORALL,v,s)::quants, cond) flow
          )
        ~felse:(fun flow ->
            Cases.singleton ([],mk_true f.range) flow
          )
        man flow

    | F_exists(v,(S_resource _ as s),ff) ->
      to_prenex_formula ff man flow >>$ fun (quants,cond) flow ->
      Cases.singleton ((EXISTS,v,s)::quants, cond) flow

    | F_exists(v,(S_interval (lo,hi) as s),ff) ->
      assume
        (mk_le lo hi f.range)
        ~fthen:(fun flow ->
            man.exec (mk_add_var v f.range) flow >>%
            man.exec (mk_assume (mk_in (mk_var v f.range) lo hi f.range) f.range) >>%
            to_prenex_formula ff man >>$ fun (quants,cond) flow ->
            Cases.singleton ((EXISTS,v,s)::quants, cond) flow
          )
        ~felse:(fun flow ->
            Cases.singleton ([],mk_false f.range) flow
          )
        man flow

  (** Evaluate a quantified formula *)
  let eval_prenex_formula cond_to_stmt quants cond range man flow : 'a flow =
    let cond' =
      match quants with
      | [] -> cond
      | _ -> mk_stub_quantified_formula quants cond range
    in
    let flow = man.exec (cond_to_stmt cond' range) flow |> post_to_flow man in
    List.fold_left
      (fun acc (_,v,s) ->
         match s with
         | S_interval _ -> man.exec (mk_remove_var v range) acc |> post_to_flow man
         | S_resource _ -> acc)
      flow quants


  let eval_quantified_formula cond_to_stmt f man flow =
    to_prenex_formula f man flow |>
    Cases.reduce_result
      (fun (quants,cond) flow -> eval_prenex_formula cond_to_stmt quants cond f.range man flow)
      ~join:(Flow.join man.lattice)
      ~meet:(Flow.meet man.lattice)
      ~bottom:(Flow.remove T_cur flow)

  let rec eval_formula
      (cond_to_stmt: expr -> range -> stmt)
      (f: formula with_range)
      range
      (man:('a, unit) man)
      (flow:'a flow)
    : 'a flow =
    debug "@[<hov>eval formula@ %a@]" pp_formula f;
    match f.content with
    | F_expr e ->
      man.exec (cond_to_stmt e range) flow |> post_to_flow man

    | F_binop (AND, f1, f2) ->
      (* FIXME: when evaluating `requires: e1 and e2;`, two alarms
         maybe generated (at location of `e1` and `e2` resp.).
         These alarms should be merged into a single one. *)
      eval_formula cond_to_stmt f1 range man flow |>
      eval_formula cond_to_stmt f2 range man

    | F_binop (OR, f1, f2) ->
      let flow1 = eval_formula cond_to_stmt f1 range man flow in
      let flow2 = eval_formula cond_to_stmt f2 range man flow in
      (* Since this is a disjunction, we can remove alarms raised by one flow
         if the other one is safe *)
      (* First, get the alarms raised by each flow *)
      let new_errors1 =
        fold2zo_report
          (fun diag acc ->
             match diag.diag_kind with
             | Error | Warning ->  diag::acc
             | _ -> acc)
          (fun d acc -> acc)
          (fun d1 d2 acc ->
             match d1.diag_kind, d2.diag_kind with
             | Error, Unreachable | Warning, Unreachable ->
               d1 :: acc
             | _ -> acc
          ) (Flow.get_report flow1) (Flow.get_report flow) [] in
      let new_errors2 =
        fold2zo_report
          (fun diag acc ->
             match diag.diag_kind with
             | Error | Warning -> diag::acc
             | _ -> acc)
          (fun d acc -> acc)
          (fun d1 d2 acc ->
             match d1.diag_kind, d2.diag_kind with
             | Error, Unreachable | Warning, Unreachable ->
               d1 :: acc
             | _ -> acc
          ) (Flow.get_report flow2) (Flow.get_report flow) [] in
      let flow1,flow2 =
        match new_errors1,new_errors2 with
        | [],[] -> flow1,flow2
        | l,[] ->
          (* Here, flow1 raised alarms while flow2 is safe. So, mark the checks
             as unreachable *)
          let report =
            List.fold_left
              (fun acc diag ->
                 let diag' = { diag with diag_kind = Unreachable;
                                         diag_alarms = AlarmSet.empty } in
                 set_diagnostic diag' acc)
              (Flow.get_report flow1) l in
          Flow.set_report report flow1, flow2
        | [],l ->
          let report =
            List.fold_left
              (fun acc diag ->
                 let diag' = { diag with diag_kind = Unreachable;
                                         diag_alarms = AlarmSet.empty } in
                 set_diagnostic diag' acc)
              (Flow.get_report flow2) l in
          flow1, Flow.set_report report flow2
        | _,_ -> flow1,flow2 in
      Flow.join man.lattice flow1 flow2

    | F_binop (IMPLIES, f1, f2) ->
      let nf1 = eval_formula mk_assume (negate_formula f1) range man flow in
      let f2 = eval_formula mk_assume f1 range man flow |>
               eval_formula cond_to_stmt f2 range man
      in
      Flow.join man.lattice nf1 f2

    | F_not ff ->
      let ff' = negate_formula ff in
      eval_formula cond_to_stmt ff' range man flow

    | F_forall _
    | F_exists _ ->
      eval_quantified_formula cond_to_stmt f man flow

    | F_in (e, S_interval (l, u)) ->
      man.exec (cond_to_stmt (mk_in e l u f.range) range) flow |> post_to_flow man

    | F_in (e, S_resource res ) ->
      man.exec (cond_to_stmt (mk_stub_resource_mem e res f.range) range) flow |> post_to_flow man


  (** Initialize the parameters of the stubbed function *)
  let init_params args params range man flow =
    List.combine args params |>
    List.fold_left (fun flow (arg, param) ->
        let post = man.exec (mk_add_var param range) flow >>%
                   man.exec (mk_assign (mk_var param range) arg range) in
        post_to_flow man post
      ) flow

  (** Remove parameters from the returned flow *)
  let remove_params params range man flow =
    params |> List.fold_left (fun flow param ->
        man.exec (mk_remove_var param range) flow |> post_to_flow man
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
      man.exec (mk_assign (mk_var v call_range) addr alloc_range) flow
    )


  (** Execute a function call *)
  (* FIXME: check the purity of f *)
  let exec_local_call v f args local_range call_range man flow =
    man.exec (mk_assign
                (mk_var v local_range)
                (mk_expr (E_call(f, args)) ~etyp:v.vtyp local_range)
                local_range
             ) flow
    |> post_to_flow man


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
      man.exec stmt flow |> post_to_flow man

    | (l,u)::tl ->
      (* Check that offsets intervals are not empty *)
      let cond = List.fold_left
          (fun acc (l,u) -> log_and acc (le l u assigns.range) assigns.range)
          (le l u assigns.range) tl
      in
      assume cond
        ~fthen:(fun flow -> man.exec stmt flow)
        ~felse:(fun flow -> Post.return flow)
        man flow |>
      post_to_flow man



  (** Remove locals *)
  let clean_post locals range man flow =
    let block =
      List.fold_left (fun block l ->
          mk_remove_var l.content.lvar range :: block
        ) [] locals
    in
    man.exec (mk_block block range) flow |> post_to_flow man


  let exec_free free range man flow =
    let e = free.content in
    let stmt = mk_stub_free e range in
    man.exec stmt flow |> post_to_flow man


  let exec_message msg range man flow =
    if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
    then flow
    else match msg.content.message_kind with
      | WARN ->
        Exceptions.warn_at range "%s" msg.content.message_body;
        flow

      | UNSOUND ->
        Flow.add_local_assumption (Soundness.A_stub_soundness_message msg.content.message_body) range flow

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
    else man.exec (mk_stub_prepare_all_assigns assigns range) flow |> post_to_flow man

  let clean_all_assigns assigns range man flow =
    (* Check if there are assigned variables *)
    if assigns = []
    then flow
    else man.exec (mk_stub_clean_all_assigns assigns range) flow |> post_to_flow man

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
        | Some (p,a) -> Keep { exp with ehistory = a :: exp.ehistory }
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
  let eval exp man flow =
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
          let flow = man.exec (mk_add_var return exp.erange) flow |> post_to_flow man in
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
          man.eval (mk_var v exp.erange) flow |>
          Cases.add_cleaners (mk_remove_var v exp.erange :: cleaners) |>
          OptionExt.return
      end

    | _ -> None


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec stmt man flow =
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


  (** Pretty printer *)
  (** ============== *)

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
