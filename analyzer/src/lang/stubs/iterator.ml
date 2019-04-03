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
open Universal.Ast
open Ast
open Zone
open Alarms

module Domain =
struct

  let name = "stubs.iterator"

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = [Z_stubs]; uses = []};
    ieval = {provides = [Z_stubs, Z_any]; uses = []};
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None


  (** Evaluation of expressions *)
  (** ========================= *)

  (** Evaluate a formula into a disjunction of two flows, depending on
     its truth value *)
  let rec eval_formula
      (f: formula with_range)
      ~(negate: bool)
      (man:('a, unit) man)
      (flow:'a flow)
    : 'a flow * 'a flow option =
    debug "eval formula %a@\n%a" pp_formula f (Flow.print man.lattice) flow;
    match f.content with
    | F_expr e ->
      man.exec (mk_assume e f.range) flow,
      (if not negate then None else Some (man.exec (mk_assume (mk_not e f.range) f.range) flow))

    | F_binop (AND, f1, f2) ->
      let ftrue1, ffalse1 = eval_formula f1 ~negate man flow in
      let ftrue, ffalse2 = eval_formula f2 ~negate man ftrue1 in

      let ffalse =
        match negate, ffalse1, ffalse2 with
        | false, None, None      -> None
        | true, Some f1, Some f2 -> Some (Flow.join man.lattice f1 f2)
        | _ -> assert false
      in

      ftrue, ffalse

    | F_binop (OR, f1, f2) ->
      let ftrue1, ffalse1 = eval_formula f1 ~negate man flow in
      let ftrue2, ffalse2 = eval_formula f2 ~negate man flow in

      let ftrue = Flow.join man.lattice ftrue1 ftrue2 in

      let ffalse =
        match negate, ffalse1, ffalse2 with
        | false, None, None      -> None
        | true, Some f1, Some f2 -> Some (Flow.meet man.lattice f1 f2)
        | _ -> assert false
      in

      ftrue, ffalse


    | F_binop (IMPLIES, f1, f2) ->
      let ftrue1, ffalse1 = eval_formula f1 ~negate:true man flow in
      let ffalse1 = Option.none_to_exn ffalse1 in

      let ftrue2, ffalse2 = eval_formula f2 ~negate man ftrue1 in

      let ftrue = Flow.join man.lattice ffalse1 ftrue2 in

      ftrue, ffalse2


    | F_not ff ->
      let ftrue, ffalse = eval_formula ff ~negate:true man flow in

      begin match negate, ffalse with
        | false, Some f -> f, None
        | true, Some f  -> f, Some ftrue
        | _ -> assert false
      end

    | F_forall (v, s, ff) -> eval_quantified_formula FORALL v s ff ~negate f.range man flow
    | F_exists (v, s, ff) -> eval_quantified_formula EXISTS v s ff ~negate f.range man flow

    | F_in (e, S_interval (l, u)) ->
      let ftrue = man.exec (mk_assume (mk_in e l u f.range) f.range) flow in
      let ffalse =
        if not negate then None
        else Some (
            man.exec (mk_assume (
                mk_binop
                  (mk_binop e O_lt l f.range)
                  O_log_or
                  (mk_binop e O_gt u f.range)
                  f.range
              ) f.range) flow
          )
      in
      ftrue, ffalse

    | F_in (e, S_resource res ) ->
      let cond = mk_stub_resource_mem e res f.range in
      let ftrue = man.exec (mk_assume cond f.range) flow in
      let ffalse =
        if not negate then None
        else
          let cond' = mk_not cond f.range in
          Some (man.exec (mk_assume cond' f.range) flow)
      in
      ftrue, ffalse

  (** Evaluate a quantified formula and its eventual negation *)
  and eval_quantified_formula q v s f ~negate range man flow =
    (* Add [v] to the environment *)
    let flow = man.exec (mk_add_var v range) flow in

    (* Constrain the range of [v] in case of ∃ *)
    let flow =
      match s, q with
      | S_interval (l, u), EXISTS ->
        man.exec (mk_assume (mk_binop (mk_var v range) O_ge l range) range) flow |>
        man.exec (mk_assume (mk_binop (mk_var v range) O_le u range) range)

      | _ -> flow
    in


    (* Replace [v] in [ff] with a quantified expression *)
    let ff1 =
      visit_expr_in_formula
        (fun e ->
           match ekind e with
           | E_var (vv, _) when compare_var v vv = 0 ->
             Keep { e with ekind = E_stub_quantified (q, v, s) }

           | _ -> VisitParts e
        )
        f
    in

    let ftrue, _ = eval_formula ff1 ~negate:false man flow in

    let remove_quant_var flow =
      man.exec (mk_remove_var v range) flow
    in

    let ftrue = remove_quant_var ftrue in

    let ffalse =
      if not negate then None
      else
        let ff = with_range (F_not ff1) f.range in
        let flip_var_quant f =
          visit_expr_in_formula
            (fun e ->
               match ekind e with
               | E_stub_quantified(FORALL, vv, s) when compare_var v vv = 0 ->
                 VisitParts { e with ekind = E_stub_quantified(EXISTS, v, s) }

               | E_stub_quantified(EXISTS, vv, s) when compare_var v vv = 0 ->
                 VisitParts { e with ekind = E_stub_quantified(FORALL, v, s) }

               | _ -> VisitParts e
            )
            f
        in
        let f =
          match q with
          | FORALL -> with_range (F_exists (v, s, flip_var_quant ff)) range
          | EXISTS -> with_range (F_forall (v, s, flip_var_quant ff)) range
        in
        let ftrue, _ = eval_formula f ~negate:false man flow in
        let ftrue = remove_quant_var ftrue in
        Some ftrue
    in

    ftrue, ffalse

  (* We need to compute a fixpoint of a formula evaluation *)
  let eval_formula_fixpoint f ~negate man flow =
    debug "eval formula fixpoint %a" pp_formula f;
    let rec lfp (flow: 'a flow) (neg: 'a flow option) : 'a flow * 'a flow option =
      debug "fixpoint iteration";
      let flow1, neg1 = eval_formula f ~negate man flow in
      let flow1' = Flow.meet man.lattice flow flow1 in
      let neg1' = (Option.neutral2 (Flow.join man.lattice) neg neg1) in
      debug "fixpoint iteration done:@\n input: @[%a@]@\n neg: @[%a@]@\n output: @[%a@]@\n neg': @[%a@]"
        (Flow.print man.lattice) flow
        (Option.print @@ Flow.print man.lattice) neg
        (Flow.print man.lattice) flow1'
        (Option.print @@ Flow.print man.lattice) neg1'
      ;
      if Flow.subset man.lattice flow flow1' then
        flow1', neg1'
      else
        lfp flow1' neg1'
    in
    (* Unroll one time *)
    let flow, neg = eval_formula f ~negate man flow in
    lfp flow neg



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
    let ftrue, _ = eval_formula_fixpoint assumes.content ~negate:false man flow in
    ftrue

  (** Evaluate the formula of the `requires` section and add the eventual alarms *)
  let exec_requires req man flow =
    let ftrue, ffalse = eval_formula_fixpoint req.content ~negate:true man flow in
    match ffalse with
    | Some ffalse when Flow.is_bottom man.lattice ffalse ->
      ftrue

    | Some ffalse ->
      raise_alarm A_stub_invalid_require req.range ~bottom:true man ffalse |>
      Flow.join man.lattice ftrue

    | _ -> assert false


  (** Execute an allocation of a new resource *)
  let exec_local_new v res range man flow =
    man.eval (mk_stub_alloc_resource res range) flow |>
    Post.bind_eval_flow man.lattice @@ fun addr flow ->
    man.exec (mk_assign (mk_var v range) addr range) flow |>
    Post.return

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
    debug "exec %a" pp_ensures e;
    (* Replace E_stub_return expression with the fresh return variable *)
    let f =
      match return with
      | None -> debug "none"; e.content
      | Some v ->
        debug "replace return with %a" pp_var v;
        visit_expr_in_formula
          (fun e ->
             match ekind e with
             | E_stub_return -> debug "real replace";Keep { e with ekind = E_var (v, STRONG) }
             | _ -> VisitParts e
          )
          e.content
    in
    (* Evaluate ensure body and return flows that verify it *)
    let ftrue, _ = eval_formula_fixpoint f ~negate:false man flow in
    ftrue


  (** Remove locals and old copies of assigned variables *)
  let clean_post locals assigns range man flow =
    let block1 =
      List.fold_left (fun block l ->
          mk_remove_var l.content.lvar range :: block
        ) [] locals
    in
    let block2 =
      List.fold_left (fun block a ->
          let t = a.content.assign_target in
          mk_stub_rename_primed t a.content.assign_offset range :: block
        ) block1 assigns
    in
    man.exec (mk_block block2 range) flow


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
    | S_assigns _ -> flow
    | S_ensures ensures -> exec_ensures ensures return man flow
    | S_free free -> exec_free free man flow
    | S_warn warn ->
      if not (Flow.get T_cur man.lattice flow |> man.lattice.is_bottom)
      then Exceptions.warn_at warn.range "%s" warn.content;
      flow


  (** Execute the body of a case section *)
  let exec_case case return man flow =
    let flow' =
      (* Execute leaf sections *)
      List.fold_left (fun flow leaf ->
          exec_leaf leaf return man flow
        ) flow case.case_body |>
      (* Clean case post state *)
      clean_post case.case_locals case.case_assigns case.case_range man
    in
    flow'


  (** Execute the body of a stub *)
  let exec_body body return man flow =
    (* Execute leaf sections *)
    let flow = List.fold_left (fun flow section ->
        match section with
        | S_leaf leaf -> exec_leaf leaf return man flow
        | _ -> flow
      ) flow body
    in

    (* Execute case sections separately *)
    let flows = Flow.map_list_opt (fun section flow ->
        match section with
        | S_case case -> Some (exec_case case return man flow)
        | _ -> None
      ) body flow
    in

    (* Join flows *)
    match flows with
    | [] -> flow
    | _ ->
      (* FIXME: when the cases do not define a partitioning, we need
         to do something else *)
      Flow.join_list man.lattice flows


  (** Entry point of expression evaluations *)
  let eval zone exp man flow =
    match ekind exp with
    | E_stub_call (stub, args) ->
      debug "call to stub %s:@\n @[%a@]"
        stub.stub_func_name
        pp_stub_func stub
      ;

      (* Initialize parameters *)
      let flow = init_params args stub.stub_func_params exp.erange man flow in

      (* Create the return variable *)
      let return, flow =
        match stub.stub_func_return_type with
        | None -> None, flow
        | Some t ->
          let return = mktmp ~typ:t () in
          let flow = man.exec (mk_add_var return exp.erange) flow in
          Some return, flow
      in

      (* Update the callstack *)
      let cs = Callstack.get flow in
      let flow = Callstack.push stub.stub_func_name exp.erange flow in

      (* Evaluate the body of the styb *)
      let flow = exec_body stub.stub_func_body return man flow in

      (* Clean locals and primes *)
      let flow = clean_post stub.stub_func_locals stub.stub_func_assigns stub.stub_func_range man flow in

      (* Remove parameters *)
      let flow = remove_params stub.stub_func_params exp.erange man flow in

      (* Restore the callstack *)
      let flow = Callstack.set cs flow in

      begin match return with
        | None ->
          Eval.empty_singleton flow |>
          Option.return

        | Some v ->
          Eval.singleton (mk_var v exp.erange) flow ~cleaners:[mk_remove_var v exp.erange] |>
          Option.return
      end

    | _ -> None


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_stub_init (v, stub) ->
      (* Evaluate the body of the stub *)
      let flow = exec_body stub.stub_init_body (Some v) man flow in

      (* Clean locals and primes *)
      let flow = clean_post stub.stub_init_locals [] stub.stub_init_range man flow in

      Post.return flow |>
      Option.return

    | _ -> None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Core.Sig.Simplified.Stateless.register_domain (module Domain)
