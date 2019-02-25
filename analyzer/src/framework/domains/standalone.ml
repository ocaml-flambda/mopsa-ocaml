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

(** Leaf domains have a simplified interface that gives access to
   their local abstractions only; the global manager and its flow
   abstraction are not accessible. *)

open Core
open Ast
open Manager
open Domain
open Eq
include Channel

module type S =
sig

  include Lattice.LATTICE

  val name : string

  val init : Ast.program -> t

  val zone : Zone.zone

  val exec : Ast.stmt -> t -> t with_channel

  val ask  : 'r Query.query -> t -> 'r option

end


(** Create a full domain from a leaf one. *)
module Make(D: S) : Domain.DOMAIN =
struct

  include D

  include Core.Id.GenDomainId(struct
      type typ = t
      let name = name
    end)

  let init prog man flow =
    Some (
      {
        flow = Flow.set_domain_env T_cur (D.init prog) man flow;
        callbacks = []
      }
    )

  let exec_interface = {
    export = [D.zone];
    import = [];
  }

  let eval_interface = {
    export = [Zone.any_zone, D.zone];
    import = [Zone.any_zone, D.zone];
  }

  let exec zone stmt man flow =
    match skind stmt with
    | S_assign(v, e) ->
      Some (
        man.eval ~zone:(Zone.any_zone, D.zone) e flow |>
        Post.bind man @@ fun e' flow ->
        let stmt' = {stmt with skind = S_assign(v, e')} in
        let flow', channels = Channel.map_domain_env T_cur (D.exec stmt') man flow in
        Post.of_flow flow' |>
        Post.add_channels channels
      )

    | S_assume(e) ->
      Some (
        man.eval ~zone:(Zone.any_zone, D.zone) e flow |>
        Post.bind man @@ fun e' flow ->
        let stmt' = {stmt with skind = S_assume(e')} in
        let flow', channels = Channel.map_domain_env T_cur (D.exec stmt') man flow in
        Post.of_flow flow' |>
        Post.add_channels channels
      )

    | S_add _ | S_remove _ | S_rename _ | S_project _ | S_fold _ | S_expand _ | S_forget _
      ->
      Some (
        let flow', channels = Channel.map_domain_env T_cur (D.exec stmt) man flow in
        Post.of_flow flow' |>
        Post.add_channels channels
      )

    | _ ->
      None

  let eval zone exp man flow = None

  let ask query man flow =
    D.ask query (Flow.get_domain_env T_cur man flow)

end



let register_domain modl =
  let module M = (val modl : S) in
  let module D = Make(M) in
  Domain.register_domain (module D)
