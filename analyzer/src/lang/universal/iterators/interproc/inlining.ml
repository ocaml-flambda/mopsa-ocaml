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
open Framework.Core.Sig.Domain.Stateless
open Ast
open Zone
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


  (** Zoning definition *)
  let interface = {
    iexec = { provides = [Z_u]; uses = [] };
    ieval = { provides = [Z_u, Z_any]; uses = [] };
  }


  (** Initialization *)
  (** ============== *)

  let init prog man (flow: 'a flow) =
    Flow.set_ctx (
      Flow.get_ctx flow |>
      Context.add_unit Callstack.ctx_key Callstack.empty
    ) flow

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow =
    let range = stmt.srange in
    match skind stmt with
    | S_return (Some e) ->
      let ret, rrange = Context.find_unit return_key (Flow.get_ctx flow) in
      let flow =
        man.exec (mk_add_var ret rrange) flow |>
        man.exec (mk_assign (mk_var ret rrange) e range) in
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (range, true)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> Option.return

    | S_return None ->
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (range, false)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> Option.return

    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)
  let eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_call({ekind = E_function (User_defined f)}, args) ->
      let params, body, flow = init_fun_params f args range man flow in
      let ret = match f.fun_return_type with
        | None -> None
        | Some _ -> Some (mk_return_var f range)
      in
      inline f params body ret range man flow
      |> Option.return

    | _ -> None

  let ask _ _ _ = None

end


let () =
  register_domain (module Domain)
