(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Control flow abstraction for switch statements. *)

open Framework.Essentials
open Ast

let name = "c.flows.switch"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                            {2 Flow tokens}                              *)
(*==========================================================================*)

type token +=
  | TSwitch
  (** Switch environments uncaught by previous cases. *)



(*==========================================================================*)
(**                             {2 Annotation}                              *)
(*==========================================================================*)

type ('a, _) Annotation.key +=
  | KSwitchExpr: ('a, expr) Annotation.key
  (** Condition expression of the most enclosing switch statement. *)



(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_switch : unit domain
  let id = D_c_switch
  let name = "c.iterators.switch"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_switch -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = [Zone.Z_c]; import = []}
  let eval_interface = {export = []; import = []}

  (** Initialization *)
  (** ============== *)

  let init prog man (flow: 'a flow) =
    Some (
      flow |> Flow.map_all_annot (fun annot ->
          Annotation.(register_annot {
              eq = (let f: type b. ('a, b) key -> (expr, b) eq option =
                      function
                      | KSwitchExpr -> Some Eq
                      | _ -> None
                    in
                    f);
            }) annot
        )
    )

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec stmt man flow =
    match skind stmt with
    | S_c_switch(e, body) ->

      (* Save current switch expression to be stored back in flow at
         the end of body of switch *)
      let cur_switch_expr =
        try
          Flow.get_annot KSwitchExpr flow |> Option.return
        with
        | Not_found -> None
      in

      (* Save T_cur env in TSwitch token, then reset T_cur and T_break. *)
      let cur = Flow.get T_cur man flow in
      let flow0 = Flow.set TSwitch cur man flow |>
                  Flow.remove T_cur man |>
                  Flow.remove Universal.Iterators.Loops.T_break man
      in

      (* Store e in the annotations and execute body. *)
      let flow0' = Flow.set_annot KSwitchExpr e flow0 in

      (* let ctx = set_annot KSwitchExpr e ctx in *)
      let flow1 = man.exec body flow0' in

      (* Merge resulting T_cur, T_break and TSwitch (in case when there is no default case) *)

      let cur =
        man.join
          (Flow.get_all_annot flow1)
          (Flow.get T_cur man flow1)
          (Flow.get Universal.Iterators.Loops.T_break man flow1) |>
        man.join
          (Flow.get_all_annot flow1)
          (Flow.get TSwitch man flow1)

      in
      (* Put the merge into T_cur and restore previous T_break and TSwitch. *)
      let flow2 = Flow.set T_cur cur man flow1 |>
                  Flow.set
                    Universal.Iterators.Loops.T_break
                    (Flow.get Universal.Iterators.Loops.T_break man flow) man |>
                  Flow.set TSwitch (Flow.get TSwitch man flow) man
      in

      (* Puts back switch expression *)
      let flow3 = match cur_switch_expr with
        | None -> Flow.rm_annot KSwitchExpr flow2
        | Some e -> Flow.set_annot KSwitchExpr e flow2
      in

      Some (Post.of_flow flow3)

    | S_c_switch_case(e) ->
      (* Look up expression in switch *)
      let e0 =
        try
          Flow.get_annot KSwitchExpr flow
        with
        | Not_found -> Debug.fail "Could not find KSwitchExpr token in \
                                   annotations at %a" pp_range (srange stmt)
      in

      let flow0 = Flow.set T_cur (Flow.get TSwitch man flow) man flow in
      Post.assume
        (mk_binop e0 O_eq e stmt.srange ~etyp:u8)
        man
        ~fthen:(fun true_flow ->
            Flow.set TSwitch man.bottom man true_flow
            |> Post.of_flow)
        ~felse:(fun false_flow ->
            let cur = Flow.get T_cur man false_flow in
            Flow.set T_cur man.bottom man false_flow |>
            Flow.set TSwitch cur man |>
            Post.of_flow
          )
        ~fboth:(fun true_flow false_flow ->
            let true_cur = Flow.get T_cur man true_flow in
            let false_cur = Flow.get T_cur man false_flow in
            Flow.merge (fun tk true_env false_env ->
                match tk, true_env, false_env with
                | T_cur, _, _ -> Some true_cur
                | TSwitch, _, _ -> Some false_cur
                | _, Some env, _ | _, _, Some env -> Some env
                | _, None, None -> None
              ) man true_flow false_flow
            |> Post.of_flow
          )
        flow0 |>
      Post.map_flow (fun flow0 -> Flow.add T_cur (Flow.get T_cur man flow) man flow0) |>
      fun x -> Some x

    | S_c_switch_default ->
      let cur = man.join
          (Flow.get_all_annot flow)
          (Flow.get TSwitch man flow)
          (Flow.get T_cur man flow)
      in
      let flow = Flow.set T_cur cur man flow |>
                 Flow.set TSwitch man.bottom man
      in
      Some (Post.of_flow flow)

    | _ -> None

  (** Evaluation of expressions *)
  (** ========================= *)

  let eval exp man flow = None

  (** Answer to queries *)
  (** ================= *)

  let ask _ _ _ = None

  end

let setup () =
    Framework.Domains.Stateless.register_domain (module Domain)
