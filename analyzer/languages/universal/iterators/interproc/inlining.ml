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


  let checks = []

  (** Command-line options *)
  (** ==================== *)

  let () =
    import_shared_option
      "universal.iterators.interproc.common.renaming"
      name

  (** Initialization *)
  (** ============== *)

  let init prog man (flow: 'a flow) =
    Flow.set_ctx (
      Flow.get_ctx flow |>
      add_ctx Context.callstack_ctx_key empty_callstack
    ) flow |>
    Post.return |>
    Option.some

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec stmt man flow =
    let range = stmt.srange in
    match skind stmt with
    | S_return (Some e) ->
      let ret = find_ctx return_key (Flow.get_ctx flow) in
      let flow = man.exec (mk_add_marker (M_return range) range) flow |> post_to_flow man in
      man.exec (mk_add_var ret range) flow >>%? fun flow ->
      man.exec (mk_assign (mk_var ret range) e range) flow >>%? fun flow ->
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (range)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> OptionExt.return

    | S_return None ->
      let flow = man.exec (mk_add_marker (M_return range) range) flow |> post_to_flow man in
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return range) cur man.lattice flow |>
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
      then Cases.empty flow |> OptionExt.return
      else

      let params, locals, body, post = init_fun_params f args range man flow in
      let ret = match f.fun_return_type with
        | None -> None
        | Some _ ->
          match f.fun_return_var with
          | Some v -> Some v
          | None   -> Some (mk_return exp)
      in
      post >>% inline f params locals body ret range man |>
      Option.some

    | _ -> None

  let ask _ _ _ = None

  let print_expr _ _ _ _ = ()

end


let () =
  register_stateless_domain (module Domain)
