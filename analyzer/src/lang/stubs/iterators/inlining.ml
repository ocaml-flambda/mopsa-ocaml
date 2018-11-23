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

  let eval_formula
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

    | F_binop (AND, f1, f2) -> panic "F_binop (AND, f1, f2) not implemented"

    | F_binop (OR, f1, f2) -> panic "F_binop (OR, f1, f2) not implemented"

    | F_binop (IMPLIES, f1, f2) -> panic "F_binop (IMPLIES, f1, f2) not implemented"

    | F_not ff -> panic "F_not ff not implemented"

    | F_forall (v, s, ff) -> panic "F_forall (v, s, ff) not implemented"

    | F_exists (v, s, ff) -> panic "F_exists (v, s, ff) not implemented"

    | F_in (v, s) -> panic "F_in (v, s) not implemented"

    | F_free(e) -> panic "F_free(e) not implemented"

  let init_params args params range man flow =
        List.combine args params |>
        List.fold_left (fun flow (arg, param) ->
            man.exec (mk_assign (mk_var param range) arg range) flow
          ) flow
          
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

  let eval_post post man flow = panic "eval_post not supported"

  let eval_body body man flow =
    match body with
    | B_post post -> eval_post post man flow
    | B_cases cases -> panic "eval_body(B_cases cases) not supported"

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

      eval_body stub.stub_body man flow2

    | _ -> None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
