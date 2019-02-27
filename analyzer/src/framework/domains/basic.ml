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

(** Basic domains have a simplified interface that gives access to
    their local abstractions only; the global manager and its flow
    abstraction are not accessible. *)

open Core
open Ast
open Manager
open Domain
open Eq

module type LEAF =
sig

  include Lattice.LATTICE

  val name : string

  val init : Ast.program -> t

  val zone : Zone.zone

  val exec : Ast.stmt -> t -> t

  val ask  : 'r Query.query -> t -> 'r option

end


(** Create a full leaf domain from a basic one. *)
module Make(D: LEAF) : Domain.LEAF =
struct

  include D

  include Core.Id.GenDomainId(struct
      type typ = t
      let name = name
    end)

  let init prog man flow =
    Some (
      Manager.set_domain_env Flow.T_cur (D.init prog) man flow
    )

  let exec_interface = {
    provides = [D.zone];
    uses = [];
  }

  let eval_interface = {
    provides = [Zone.any_zone, D.zone];
    uses = [Zone.any_zone, D.zone];
  }

  let exec zone stmt man flow =
    match skind stmt with
    | S_assign(v, e) ->
      Some (
        man.eval ~zone:(Zone.any_zone, D.zone) e flow |>
        Post.bind_eval @@ fun e' flow ->
        let stmt' = {stmt with skind = S_assign(v, e')} in
        Manager.map_domain_env Flow.T_cur (D.exec stmt') man flow |>
        Post.of_flow
      )

    | S_assume(e) ->
      Some (
        man.eval ~zone:(Zone.any_zone, D.zone) e flow |>
        Post.bind_eval @@ fun e' flow ->
        let stmt' = {stmt with skind = S_assume(e')} in
        Manager.map_domain_env Flow.T_cur (D.exec stmt') man flow |>
        Post.of_flow
      )

    | S_add _ | S_remove _ | S_rename _ | S_project _ | S_fold _ | S_expand _ | S_forget _
      ->
      Some (
        Manager.map_domain_env Flow.T_cur (D.exec stmt) man flow |>
        Post.of_flow
      )

    | _ ->
      None

  let eval zone exp man flow = None

  let ask query man flow =
    D.ask query (Manager.get_domain_env Flow.T_cur man flow)

end



let register_leaf_domain modl =
  let module M = (val modl : LEAF) in
  let module D = Make(M) in
  Domain.register_leaf_domain (module D)
