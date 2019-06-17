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


(** The operator [Sequence ∈ 𝒟 × 𝒟 → 𝒟] operator combines two domains by
    "concatenating" their transfer functions (i.e. return the result of the
    first answering domain).
*)


open Ast.All
open Core.All
open Log
open Sig.Domain.Stateless

module Make
    (D1:DOMAIN)
    (D2:DOMAIN)
  : DOMAIN
=
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  include GenStatelessDomainId(struct
      let name = "framework.transformers.domain.stateless.sequence"
    end)

  let interface = Core.Interface.concat D1.interface D2.interface


  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow =
    D1.init prog man flow |>
    D2.init prog man

  (** Execution of statements *)
  let exec zone =
    match Core.Interface.sat_exec zone D1.interface,
          Core.Interface.sat_exec zone D2.interface
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [exec] for such zone *)
      D1.exec zone

    | false, true ->
      (* Only [D2] provides an [exec] for such zone *)
      D2.exec zone

    | true, true ->
      (* Both [D1] and [D2] provide an [exec] for such zone *)
      let f1 = D1.exec zone in
      let f2 = D2.exec zone in
      (fun stmt man flow ->
         match f1 stmt man flow with
         | Some post -> Some post

         | None -> f2 stmt man flow
      )


  (** Evaluation of expressions *)
  let eval zone =
    match Core.Interface.sat_eval zone D1.interface,
          Core.Interface.sat_eval zone D2.interface
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [eval] for such zone *)
      D1.eval zone

    | false, true ->
      (* Only [D2] provides an [eval] for such zone *)
      D2.eval zone

    | true, true ->
      (* Both [D1] and [D2] provide an [eval] for such zone *)
      let f1 = D1.eval zone in
      let f2 = D2.eval zone in
      (fun exp man flow ->
         match f1 exp man flow with
         | Some evl -> Some evl

         | None -> f2 exp man flow
      )


  (** Query handler *)
  let ask query man flow =
    let reply1 = D1.ask query man flow in
    let reply2 = D2.ask query man flow in
    Option.neutral2 (join_query query) reply1 reply2


end
