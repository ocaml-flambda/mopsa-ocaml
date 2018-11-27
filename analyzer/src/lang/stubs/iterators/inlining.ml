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

  let rec eval_formula
      (f: formula with_range)
      (man:('a, unit) man)
      (flow:'a flow)
    : ('a, bool) evl =
    match f.content with
    | F_expr e ->
      Eval.assume e
        ~fthen:(fun flow -> Eval.singleton true flow)
        ~felse:(fun flow -> Eval.singleton false flow)
        man flow

    | F_binop (AND, f1, f2) ->
      eval_formula f1 man flow |>
      Eval.bind @@ fun b1 flow1 ->

      eval_formula f2 man flow |>
      Eval.bind @@ fun b2 flow2 ->

      Eval.singleton (b1 && b2) (Flow.meet man flow1 flow2)

    | F_binop (OR, f1, f2) ->
      eval_formula f1 man flow |>
      Eval.bind @@ fun b1 flow1 ->

      eval_formula f2 man flow |>
      Eval.bind @@ fun b2 flow2 ->

      Eval.singleton (b1 || b2) (Flow.join man flow1 flow2)


    | F_binop (IMPLIES, f1, f2) ->
      eval_formula f1 man flow |>
      Eval.bind @@ fun b1 flow1 ->

      eval_formula f2 man flow |>
      Eval.bind @@ fun b2 flow2 ->

      Eval.singleton ((not b1) || b2) (if b1 then Flow.meet man flow1 flow2 else flow1)


    | F_not ff ->
      eval_formula ff man flow |>
      Eval.bind @@ fun b flow ->

      Eval.singleton (not b) flow

    | F_forall (v, s, ff) -> panic "F_forall (v, s, ff) not implemented"

    | F_exists (v, s, ff) -> panic "F_exists (v, s, ff) not implemented"

    | F_in (v, s) -> panic "F_in (v, s) not implemented"

    | F_free(e) -> panic "F_free(e) not implemented"

  (** Initialize the parameters of the stubbed function *)
  let init_params args params range man flow =
        List.combine args params |>
        List.fold_left (fun flow (arg, param) ->
            man.exec (mk_assign (mk_var param range) arg range) flow
          ) flow

  (** Evaluate the formula of the `requires` section and add the eventual alarms *)
  let check_requirements reqs range man flow =
    List.fold_left (fun flow req ->
        eval_formula req.content man flow |>
        Post.bind_flow man @@ fun b flow ->
        if b then
          (* valid requirement formula *)
          flow
        else
          (* invalid requirement formula *)
          let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
          let alarm = mk_alarm (A_stub_invalid_require req) range ~cs in
          Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
          Flow.remove T_cur man
      ) flow reqs

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

  let exec_ensures e man flow =
    eval_formula e.content man flow |>
    Post.bind_flow man @@ fun b flow ->
    if b then flow
    else Flow.remove T_cur man flow


  (** Compute a post-condition *)
  let exec_post post retyp man flow =
    (* Execute `assigns` section *)
    let flow = List.fold_left (fun flow a -> exec_assigns a man flow) flow post.post_assigns in
    (* Execute `local` section *)
    let flow = List.fold_left (fun flow l -> exec_local l man flow) flow post.post_local in
    (* Execute `ensures` section *)
    let rec doit = function
      | [] -> flow
      | [e] -> exec_ensures e man flow
      | e :: tl ->
        exec_ensures e man flow |>
        Flow.meet man (doit tl)
    in
    doit post.post_ensures

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
      let flow2 = check_requirements stub.stub_requires exp.erange man flow1 in

      (* Create the return variable *)
      let return = mk_tmp ~vtyp:stub.stub_return_type () in
      let flow3 = man.exec (mk_add_var return exp.erange) flow2 in

      (* Evaluate the body of the styb *)
      let flow4 = exec_body stub.stub_body return man flow3 in

      Eval.singleton (mk_var return exp.erange) flow4 ~cleaners:[mk_remove_var return exp.erange] |>
      Eval.return

    | _ -> None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
