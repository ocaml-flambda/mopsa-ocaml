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

(** Sequence of stateless domains *)

open Ast.All
open Core.All
open Sig.Combiner.Stateless
open Common


module Make(C1:STATELESS_COMBINER)(C2:STATELESS_COMBINER) : STATELESS_COMBINER =
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  let id = C_empty

  let name = C1.name ^ " ; " ^ C2.name

  let nodes = C1.nodes @ C2.nodes

  let dependencies = C1.dependencies @ C2.dependencies

  let wirings =
    List.fold_left
      (fun acc sem1 -> add_wirings sem1 C2.nodes acc)
      (join_wirings C1.wirings C2.wirings)
      C1.dependencies

  let alarms = C1.alarms @ C2.alarms |> List.sort_uniq compare

  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow =
    C1.init prog man flow |>
    C2.init prog man

  (** Execution of statements *)
  let exec domains =
    match sat_targets ~targets:domains ~nodes:C1.nodes,
          sat_targets ~targets:domains ~nodes:C2.nodes
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [exec] for such zone *)
      C1.exec domains

    | false, true ->
      (* Only [D2] provides an [exec] for such zone *)
      C2.exec domains

    | true, true ->
      (* Both [D1] and [D2] provide an [exec] for such zone *)
      let f1 = C1.exec domains in
      let f2 = C2.exec domains in
      (fun stmt man flow ->
         match f1 stmt man flow with
         | Some post -> Some post

         | None -> f2 stmt man flow
      )


  (** Evaluation of expressions *)
  let eval domains =
    match sat_targets ~targets:domains ~nodes:C1.nodes,
          sat_targets ~targets:domains ~nodes:C2.nodes
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [eval] for such zone *)
      C1.eval domains

    | false, true ->
      (* Only [D2] provides an [eval] for such zone *)
      C2.eval domains

    | true, true ->
      (* Both [D1] and [D2] provide an [eval] for such zone *)
      let f1 = C1.eval domains in
      let f2 = C2.eval domains in
      (fun exp man flow ->
         match f1 exp man flow with
         | Some evl -> Some evl

         | None -> f2 exp man flow
      )


  (** Query handler *)
  let ask domains =
match sat_targets ~targets:domains ~nodes:C1.nodes,
      sat_targets ~targets:domains ~nodes:C2.nodes
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [eval] for such zone *)
      C1.ask domains

    | false, true ->
      (* Only [D2] provides an [eval] for such zone *)
      C2.ask domains

    | true, true ->
      (* Both [D1] and [D2] provide an [eval] for such zone *)
      let f1 = C1.ask domains in
      let f2 = C2.ask domains in
      (fun q man flow ->
         OptionExt.neutral2
           (join_query q ~join:(fun a b -> man.lattice.join (Flow.get_unit_ctx flow) a b))
           (f1 q man flow)
           (f2 q man flow)
      )

end



let rec make (domains:(module STATELESS_COMBINER) list) : (module STATELESS_COMBINER) =
  match domains with
  | [] -> assert false
  | [d] -> d
  | l ->
    let a,b = ListExt.split l in
    let aa, bb = make a, make b in
    (module Make(val aa : STATELESS_COMBINER)(val bb : STATELESS_COMBINER))
