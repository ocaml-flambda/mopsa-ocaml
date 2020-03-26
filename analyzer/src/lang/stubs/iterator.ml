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
open Framework.Core.Sig.Domain.Stateless
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


  let join_alarms f1 f2 flow =
    match AlarmSet.subset (Flow.get_alarms f1) (Flow.get_alarms flow),
          AlarmSet.subset (Flow.get_alarms f2) (Flow.get_alarms flow)
    with
    | false,true
    | true,false ->
      (* Only one evaluation detected alarms, so ignore them *)
      Flow.get_alarms flow

    | true, true
    | false, false ->
      (* Both evaluations behave similarly, so keep them *)
      AlarmSet.union (Flow.get_alarms f1) (Flow.get_alarms f2)



  let rec eval_formula
      (cond_to_stmt: expr -> range -> stmt)
      (f: formula with_range)
      (man:('a, unit) man)
      (flow:'a flow)
    : 'a flow =
    debug "@[<hov>eval formula@ %a@]" pp_formula f;
    match f.content with
    | F_expr e ->
      man.exec (cond_to_stmt e f.range) flow

    | F_binop (AND, f1, f2) ->
      eval_formula cond_to_stmt f1 man flow |>
      eval_formula cond_to_stmt f2 man

    | F_binop (OR, f1, f2) ->
      let f1 = eval_formula cond_to_stmt f1 man flow in
      let f2 = eval_formula cond_to_stmt f2 man flow in
      let alarms = join_alarms f1 f2 flow in
      Flow.join man.lattice f1 f2 |>
      Flow.set_alarms alarms


    | F_binop (IMPLIES, f1, f2) ->
      let nf1 = eval_formula mk_assume (negate_formula f1) man flow in
      let f2 = eval_formula mk_assume f1 man flow |>
               eval_formula cond_to_stmt f2 man
      in
      Flow.join man.lattice nf1 f2

    | F_not ff ->
      let ff' = negate_formula ff in
      eval_formula cond_to_stmt ff' man flow

    | F_forall (v, s, ff) ->
      eval_quantified_formula cond_to_stmt FORALL v s ff f.range man flow

    | F_exists (v, s, ff) ->
      eval_quantified_formula cond_to_stmt EXISTS v s ff f.range man flow

    | F_in (e, S_interval (l, u)) ->
      man.exec (cond_to_stmt (mk_in e l u f.range) f.range) flow

    | F_in (e, S_resource res ) ->
      man.exec (cond_to_stmt (mk_stub_resource_mem e res f.range) f.range) flow


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
          let flow = eval_formula cond_to_stmt f' man flow in
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
  let exec_assumes assumes man flow =
    eval_formula mk_assume assumes.content man flow


  (** Evaluate the formula of the `requires` section *)
  let exec_requires req man flow =
    eval_formula mk_stub_requires req.content man flow


  (** Execute an allocation of a new resource *)
  let exec_local_new v res range man flow : 'a flow =
    (* Evaluation the allocation request *)
    post_to_flow man (
      man.eval (mk_stub_alloc_resource res range) flow >>$ fun addr flow ->

      (* Add the address dimension before doing the assignment *)
      (
        match ekind addr with
        | E_addr _ -> man.post (mk_add addr range) flow
        | _ -> Post.return flow
      )
      >>$ fun _ flow ->

      (* Assign the address to the variable *)
      man.post (mk_assign (mk_var v range) addr range) flow
    )


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
    man.exec (mk_stub_assigns
                assigns.content.assign_target
                assigns.content.assign_offset
                assigns.range
             ) flow


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


  (** Execute a leaf section *)
  let exec_leaf leaf return man flow =
    match leaf with
    | S_local local -> exec_local local man flow
    | S_assumes assumes -> exec_assumes assumes man flow
    | S_requires requires -> exec_requires requires man flow
    | S_assigns assigns -> exec_assigns assigns man flow
    | S_ensures ensures -> exec_ensures ensures return man flow
    | S_free free -> exec_free free man flow
    | S_warn warn ->
      if not (Flow.get T_cur man.lattice flow |> man.lattice.is_bottom)
      then Exceptions.warn_at warn.range "%s" warn.content;
      flow


  (** Execute the body of a case section *)
  let exec_case case return man flow =
    List.fold_left (fun acc leaf ->
        exec_leaf leaf return man acc
      ) flow case.case_body |>

    (* Clean case post state *)
    clean_post case.case_locals case.case_range man


  (** Execute the body of a stub *)
  let exec_body ?(stub=None) body return man flow =
    (* Execute leaf sections *)
    let flow = List.fold_left (fun flow section ->
        match section with
        | S_leaf leaf -> exec_leaf leaf return man flow
        | _ -> flow
      ) flow body
    in

    (* Execute case sections separately *)
    let flows, ctx = List.fold_left (fun (acc,ctx) section ->
        match section with
        | S_case case when not (is_case_ignored stub case) ->
          let flow = Flow.set_ctx ctx flow in
          let flow' = exec_case case return man flow in
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
      let flow = exec_body ~stub:(Some stub) stub.stub_func_body return man flow in

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
      let flow = exec_body stub.stub_directive_body None man flow in

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
  register_domain (module Domain)
