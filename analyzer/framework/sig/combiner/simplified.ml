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

(** Extended domains signatures used by combiners *)

open Core.All
open Abstraction.Simplified
open Mopsa_utils

module type SIMPLIFIED_COMBINER =
sig
  include SIMPLIFIED
  val domains : DomainSet.t
  val semantics : SemanticSet.t
  val routing_table : routing_table
  val merge : t -> t * teffect -> t * teffect -> t
  val exec : DomainSet.t option -> stmt -> ('a,t) simplified_man -> 'a ctx -> t -> t option
  val ask  : DomainSet.t option -> ('a,'r) query -> ('a,t) simplified_man -> 'a ctx -> t -> 'r option
  val print_state : DomainSet.t option -> printer -> t -> unit
  val print_expr  : DomainSet.t option -> ('a,t) simplified_man -> 'a ctx -> t -> printer -> expr -> unit
end



module SimplifiedToCombiner(D:SIMPLIFIED) : SIMPLIFIED_COMBINER with type t = D.t =
struct
  include D
  let domains = DomainSet.singleton D.name
  let semantics = SemanticSet.empty
  let routing_table = empty_routing_table
  let merge pre (a1,te1) (a2,te2) =
    let e1 = get_root_effect te1 in
    let e2 = get_root_effect te2 in
    if compare_effect e1 e2 = 0 then a1 else
    if is_empty_effect e1 then a2 else
    if is_empty_effect e2 then a1
    else D.merge pre (a1,e1) (a2,e2)
  let exec domains = D.exec
  let ask domains  = D.ask
  let print_state targets = D.print_state
  let print_expr  targets = D.print_expr
end

module CombinerToSimplified(T:SIMPLIFIED_COMBINER) : SIMPLIFIED with type t = T.t =
struct
  include T
  let merge pre (a1,e1) (a2,e2) =
    let te1 = mk_teffect e1 empty_teffect empty_teffect in
    let te2 = mk_teffect e2 empty_teffect empty_teffect in
    T.merge pre (a1,te1) (a2,te2)
  let exec stmt man ctx a = T.exec None stmt man ctx a
  let ask query man ctx a = T.ask None query man ctx a
  let print_state printer a = T.print_state None printer a
  let print_expr man ctx a printer e = T.print_expr None man ctx a printer e
end



module SimplifiedToStandard(D: SIMPLIFIED_COMBINER) : Domain.DOMAIN_COMBINER with type t = D.t =
struct

  include D

  let init prog man flow =
    let a' = D.init prog in
    set_env T_cur a' man flow

  let routing_table = empty_routing_table

  let checks = []

  let simplified_man man flow = {
    exec = (fun stmt -> man.Core.Manager.exec stmt flow |>
                        post_to_flow man |>
                        get_env T_cur man
           );
    ask = (fun query -> man.Core.Manager.ask query flow);
  }

  let exec domains =
    let f = D.exec domains in
    (fun stmt man flow ->
       let a = get_env T_cur man flow in
       if D.is_bottom a
       then
         Post.return flow |>
         OptionExt.return
       else
         f stmt (simplified_man man flow) (Flow.get_ctx flow) a |>
         OptionExt.lift @@ fun a' ->
         set_env T_cur a' man flow |>
         Post.return |>
         Cases.map_effects (fun effects ->
             man.set_effects (
               man.get_effects effects |>
               add_stmt_to_teffect stmt
             ) effects
           )
    )

  let eval domains exp man flow = None

  let ask domains =
    let f = D.ask domains in
    (fun query man flow ->
       f query (simplified_man man flow) (Flow.get_ctx flow) (get_env T_cur man flow)
    )

  let print_expr domains =
    let f = D.print_expr domains in
    (fun man flow printer e ->
       let a = get_env T_cur man flow in
       if D.is_bottom a
       then ()
       else f (simplified_man man flow) (Flow.get_ctx flow) a printer e
    )
end

