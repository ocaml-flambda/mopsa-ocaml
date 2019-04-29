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

(** Control flow abstraction for switch statements. *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast

(*==========================================================================*)
(**                            {2 Flow tokens}                              *)
(*==========================================================================*)

type token +=
  | TSwitch
  (** Switch environments uncaught by previous cases. *)

let () =
  register_token {
    compare = (fun next tk -> next tk);
    print   = (fun next fmt tk ->
        match tk with
        | TSwitch -> Format.fprintf fmt "switch"
        | _ -> next fmt tk
      );
  };
  ()


(*==========================================================================*)
(**                     {2 Flow-insensitive context}                        *)
(*==========================================================================*)

let switch_expr_ctx =
  let module C = Context.GenUnitKey(
    struct

      type t = expr
      (** Condition expression of the most enclosing switch statement. *)

      let print fmt cond =
        Format.fprintf fmt "Switch expr: %a" pp_expr cond
    end
    )
  in
  C.key

let get_switch_cond flow =
  Flow.get_ctx flow |>
  Context.find_unit switch_expr_ctx

let set_switch_cond cond flow =
  Flow.set_ctx (
    Flow.get_ctx flow |>
    Context.add_unit switch_expr_ctx cond
  ) flow

let remove_switch_cond flow =
  Flow.set_ctx (
    Flow.get_ctx flow |>
    Context.remove_unit switch_expr_ctx
  ) flow

(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  let name = "c.iterators.switch"
  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = [Zone.Z_c]; uses = []};
    ieval = {provides = []; uses = []};
  }

  (** Initialization *)
  (** ============== *)

  let init _ _ flow =  flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_c_switch(e, body) ->

      (* Save current switch expression to be stored back in flow at
         the end of body of switch *)
      let cur_switch_expr =
        try Some (get_switch_cond flow)
        with Not_found -> None
      in

      (* Save T_cur env in TSwitch token, then reset T_cur and T_break. *)
      let cur = Flow.get T_cur man.lattice flow in
      let flow0 = Flow.set TSwitch cur man.lattice flow |>
                  Flow.remove T_cur |>
                  Flow.remove Universal.Iterators.Loops.T_break
      in

      (* Store e in the annotations and execute body. *)
      let flow0' = set_switch_cond e flow0 in

      (* let ctx = set_annot KSwitchExpr e ctx in *)
      let flow1 = man.exec body flow0' in

      (* Merge resulting T_cur, T_break and TSwitch (in case when there is no default case) *)
      let cur =
        man.lattice.join
          (Flow.get T_cur man.lattice flow1)
          (Flow.get Universal.Iterators.Loops.T_break man.lattice flow1) |>
        man.lattice.join
          (Flow.get TSwitch man.lattice flow1)

      in
      (* Put the merge into T_cur and restore previous T_break and TSwitch. *)
      let flow2 = Flow.set T_cur cur man.lattice flow1 |>
                  Flow.set
                    Universal.Iterators.Loops.T_break
                    (Flow.get Universal.Iterators.Loops.T_break man.lattice flow) man.lattice |>
                  Flow.set TSwitch (Flow.get TSwitch man.lattice flow) man.lattice
      in

      (* Puts back switch expression *)
      let flow3 = match cur_switch_expr with
        | None -> remove_switch_cond flow2
        | Some e -> set_switch_cond e flow2
      in

      Post.return flow3 |>
      Option.return

    | S_c_switch_case(e) ->
      (* Look up expression in switch *)
      let e0 =
        try get_switch_cond flow
        with Not_found -> Exceptions.panic_at stmt.srange
                            "Could not find KSwitchExpr token in annotations at %a"
                            pp_range (srange stmt)
      in

      let flow0 = Flow.set T_cur (Flow.get TSwitch man.lattice flow) man.lattice flow in
      assume_post
        (mk_binop e0 O_eq e stmt.srange ~etyp:u8)
        ~fthen:(fun true_flow ->
            Flow.set TSwitch man.lattice.bottom man.lattice true_flow
            |> Post.return)
        ~felse:(fun false_flow ->
            let cur = Flow.get T_cur man.lattice false_flow in
            Flow.set T_cur man.lattice.bottom man.lattice false_flow |>
            Flow.set TSwitch cur man.lattice |>
            Post.return
          )
        ~fboth:(fun true_flow false_flow ->
            let true_cur = Flow.get T_cur man.lattice true_flow in
            let false_cur = Flow.get T_cur man.lattice false_flow in
            Flow.merge (fun tk true_env false_env ->
                match tk, true_env, false_env with
                | T_cur, _, _ -> Some true_cur
                | TSwitch, _, _ -> Some false_cur
                | _, Some env, _ | _, _, Some env -> Some env
                | _, None, None -> None
              ) man.lattice true_flow false_flow
            |> Post.return
          )
        man flow0 |>
      Post.bind (fun flow0 ->
          Flow.add T_cur (Flow.get T_cur man.lattice flow) man.lattice flow0 |>
          Post.return
        ) |>
      Option.return

    | S_c_switch_default ->
      let cur = man.lattice.join
          (Flow.get TSwitch man.lattice flow)
          (Flow.get T_cur man.lattice flow)
      in
      let flow = Flow.set T_cur cur man.lattice flow |>
                 Flow.set TSwitch man.lattice.bottom man.lattice
      in
      Post.return flow |>
      Option.return

    | _ -> None

  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow = None

  (** Answer to queries *)
  (** ================= *)

  let ask _ _ _ = None

  end

let () =
    Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
