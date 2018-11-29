(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inter-procedural iterator of stubs by inlining. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Zone
open Alarms

module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_stubs_iterators_inlining : unit domain

  let id = D_stubs_iterators_inlining
  let name = "stubs.iterators.inlining"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_stubs_iterators_inlining -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [Z_stubs, Z_any]; import = []}


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow = None


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
    match f.content with
    | F_expr e ->
      man.exec (mk_assume e f.range) flow,
      (if not negate then None else Some (man.exec (mk_assume (mk_not e f.range) f.range) flow))

    | F_binop (AND, f1, f2) ->
      let ftrue1, ffalse1 = eval_formula f1 ~negate man flow in
      let ftrue2, ffalse2 = eval_formula f2 ~negate man flow in

      let ftrue = Flow.meet man ftrue1 ftrue2 in

      let ffalse =
        match negate, ffalse1, ffalse2 with
        | false, None, None      -> None
        | true, Some f1, Some f2 -> Some (Flow.join man f1 f2)
        | _ -> assert false
      in

      ftrue, ffalse

    | F_binop (OR, f1, f2) ->
      let ftrue1, ffalse1 = eval_formula f1 ~negate man flow in
      let ftrue2, ffalse2 = eval_formula f2 ~negate man flow in

      let ftrue = Flow.join man ftrue1 ftrue2 in

      let ffalse =
        match negate, ffalse1, ffalse2 with
        | false, None, None      -> None
        | true, Some f1, Some f2 -> Some (Flow.meet man f1 f2)
        | _ -> assert false
      in

      ftrue, ffalse


    | F_binop (IMPLIES, f1, f2) -> panic_at f.range "IMPLIES not supported"

    | F_not ff ->
      let ftrue, ffalse = eval_formula ff ~negate:true man flow in

      begin match negate, ffalse with
        | false, Some f -> f, None
        | true, Some f  -> f, Some ftrue
        | _ -> assert false
      end

    | F_forall (v, s, ff) -> eval_quantified_formula FORALL v s ff ~negate f.range man flow
    | F_exists (v, s, ff) -> eval_quantified_formula EXISTS v s ff ~negate f.range man flow

    | F_in (v, s) -> panic "F_in (v, s) not implemented"

    | F_free(e) -> panic "F_free(e) not implemented"

  (** Evaluate a quantified formula and its eventual negation *)
  and eval_quantified_formula q v s f ~negate range man flow =
    (* Add [v] to the environment *)
    let flow = man.exec (mk_add_var v range) flow in

    (* Initialize its value *)
    let flow =
      match s with
      | S_interval (l, u) -> man.exec (mk_assume (mk_binop (mk_var v range) O_ge l range) range) flow |>
                             man.exec (mk_assume (mk_binop (mk_var v range) O_le u range) range)
      | S_resource _ -> panic_at range "quantified resource instances not supported"
    in

    (* Replace [v] in [ff] with a quantified expression *)
    let ff1 =
      visit_expr_in_formula
        (fun e ->
           match ekind e with
           | E_var (vv, _) when compare_var v vv = 0 -> { e with ekind = E_stub_quantified (q, e) }
           | _ -> e
        )
        f
    in

    let ftrue, _ = eval_formula ff1 ~negate:false man flow in

    let ffalse =
      if not negate then None
      else
        let ff = with_range (F_not f) f.range in
        let f =
          match q with
          | FORALL -> with_range (F_exists (v, s, ff)) range
          | EXISTS -> with_range (F_forall (v, s, ff)) range
        in
        let ftrue, _ = eval_formula f ~negate:false man flow in
        Some ftrue
    in

    ftrue, ffalse


  (** Initialize the parameters of the stubbed function *)
  let init_params args params range man flow =
        List.combine args params |>
        List.fold_left (fun flow (arg, param) ->
            man.exec (mk_assign (mk_var param range) arg range) flow
          ) flow

  (** Evaluate the formula of the `requires` section and add the eventual alarms *)
  let check_requirement req man flow =
    let ftrue, ffalse = eval_formula req.content ~negate:true man flow in
    match ffalse with
    | Some f when Flow.is_bottom man f -> ftrue

    | Some f ->
      let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
      let alarm = mk_alarm (A_stub_invalid_require req) req.range ~cs in
      Flow.add (alarm_token alarm) (Flow.get T_cur man f) man ftrue

    | _ -> assert false

  (** Fold the checks of pre-conditions over a list of requirements *)
  let rec check_requirements reqs man flow =
    match reqs with
    | [] -> flow
    | [req] -> check_requirement req man flow
    | hd :: tl ->
      let flow1 = check_requirement hd man flow in
      let flow2 = check_requirements tl man flow in
      (* Meet T_cur and keep all other flows *)
      let annot = Flow.get_all_annot flow2 in
      Flow.merge
        (fun tk env1 env2 ->
           match tk with
           | T_cur -> OptionExt.option_lift2 (man.meet annot) env1 env2
           | _ -> OptionExt.option_neutral2 (man.join annot) env1 env2
        ) man flow1 flow2

  (** Execute the `assigns` section *)
  let exec_assigns a man flow =
    let stmt =
      mk_stub_assigns
        a.content.assign_target
        a.content.assign_offset
        a.range
    in
    man.exec stmt flow

  (** Execute the `local` section *)
  let exec_local l man flow =
    match l.content.lval with
    | L_new  _ -> panic "allocations not yet supported"
    | L_call _ -> panic "function calls not yet supported"

  let exec_ensures e return man flow =
    (* Replace E_stub_return expression with the fresh return variable *)
    let f =
      match return with
      | None -> e.content
      | Some v ->
        visit_expr_in_formula
          (fun e ->
             match ekind e with
             | E_stub_return -> debug "return found"; { e with ekind = E_var (v, STRONG) }
             | _ -> e
          )
          e.content
    in
    (* Evaluate ensure body and return flows that verify it *)
    eval_formula f ~negate:false man flow |>
    fst

  (** Remove locals and old copies of assigned variables *)
  let clean_post post return range man flow =
    let block1 =
      post.post_local |> List.fold_left (fun block l ->
          mk_remove_var l.content.lvar range :: block
        ) []
    in
    let block2 =
      post.post_assigns |> List.fold_left (fun block a ->
          mk_stub_remove_old a.content.assign_target range :: block
        ) block1
    in
    man.exec (mk_block block2 range) flow

  (** Compute a post-condition *)
  let exec_post post return range man flow =
    (* Execute `assigns` section *)
    let flow = List.fold_left (fun flow a -> exec_assigns a man flow) flow post.post_assigns in
    (* Execute `local` section *)
    let flow = List.fold_left (fun flow l -> exec_local l man flow) flow post.post_local in
    (* Execute `ensures` section *)
    let rec doit = function
      | [] -> flow
      | [e] -> exec_ensures e return man flow
      | e :: tl ->
        exec_ensures e return man flow |>
        Flow.meet man (doit tl)
    in
    let flow = doit post.post_ensures in
    clean_post post return range man flow

  (** Execute the body of a stub *)
  let exec_body body return man flow =
    match body with
    | B_post post -> exec_post post return man flow
    | B_cases cases -> panic "eval_body(B_cases cases) not supported"

  (** Entry point of expression evaluations *)
  let eval zone exp man flow =
    match ekind exp with
    | E_stub_call (stub, args) ->
      debug "call to stub %s:@\n @[%a@]"
        stub.stub_name
        pp_stub stub
      ;

      (* Initialize parameters *)
      let flow1 = init_params args stub.stub_params exp.erange man flow in

      (* Check requirements *)
      let flow2 = check_requirements stub.stub_requires man flow1 in

      (* Create the return variable *)
      let return, flow3 =
        match stub.stub_return_type with
        | None -> None, flow2
        | Some t ->
          let return = mk_tmp ~vtyp:t () in
          let flow3 = man.exec (mk_add_var return exp.erange) flow2 in
          Some return, flow3
      in

      (* Evaluate the body of the styb *)
      let flow4 = exec_body stub.stub_body return exp.erange man flow3 in

      begin match return with
        | None ->
          Eval.empty_singleton flow4 |>
          Eval.return

        | Some v ->
          Eval.singleton (mk_var v exp.erange) flow4 ~cleaners:[mk_remove_var v exp.erange] |>
          Eval.return
      end

    | _ -> None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
