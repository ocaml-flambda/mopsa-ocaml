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

(** Inter-procedural iterator by inlining.  *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Common


(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "universal.iterators.interproc.inlining"
    end)


  let alarms = []

  (** Initialization *)
  (** ============== *)

  let init prog man (flow: 'a flow) =
    Flow.set_ctx (
      Flow.get_ctx flow |>
      Context.add_unit Context.callstack_ctx_key empty_callstack
    ) flow

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec stmt man flow =
    let range = stmt.srange in
    match skind stmt with
    | S_return (Some e) ->
      let ret = Context.find_unit return_key (Flow.get_ctx flow) in
      man.exec (mk_add_var ret range) flow >>%? fun flow ->
      man.exec (mk_assign (mk_var ret range) e range) flow >>%? fun flow ->
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (range)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> OptionExt.return

    | S_return None ->
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (range)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> OptionExt.return

    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)
  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_call({ekind = E_function (User_defined f)}, args) ->

      if man.lattice.is_bottom (Flow.get T_cur man.lattice flow)
      then Cases.empty_singleton flow |> OptionExt.return
      else

      let params, locals, body, post = init_fun_params f args range man flow in
      let ret = match f.fun_return_type with
        | None -> None
        | Some _ -> Some (mk_return_var exp)
      in
      Some (
        let post' = post >>% inline f params locals body ret range man in
        (* FIXME: we keep logs intra-procedural for the moment *)
        Cases.clear_log post'
      )

    | _ -> None

  let ask _ _ _ = None

end


let () =
  register_stateless_domain (module Domain)
