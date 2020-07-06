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
open Tree

module Make(T1:STATELESS)(T2:STATELESS) : STATELESS =
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  let id = I_empty

  let name = T1.name ^ " ; " ^ T2.name

  let dependencies = T1.dependencies @ T2.dependencies

  let leaves = T1.leaves @ T2.leaves

  let alarms = T1.alarms @ T2.alarms |> List.sort_uniq compare

  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow =
    T1.init prog man flow |>
    T2.init prog man

  (** Execution of statements *)
  let exec targets =
    match sat_targets ~targets ~leaves:T1.leaves,
          sat_targets ~targets ~leaves:T2.leaves
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [exec] for such zone *)
      T1.exec targets

    | false, true ->
      (* Only [D2] provides an [exec] for such zone *)
      T2.exec targets

    | true, true ->
      (* Both [D1] and [D2] provide an [exec] for such zone *)
      let f1 = T1.exec targets in
      let f2 = T2.exec targets in
      (fun stmt man flow ->
         match f1 stmt man flow with
         | Some post -> Some post

         | None -> f2 stmt man flow
      )


  (** Evaluation of expressions *)
  let eval targets =
    match sat_targets ~targets ~leaves:T1.leaves,
          sat_targets ~targets ~leaves:T2.leaves
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [eval] for such zone *)
      T1.eval targets

    | false, true ->
      (* Only [D2] provides an [eval] for such zone *)
      T2.eval targets

    | true, true ->
      (* Both [D1] and [D2] provide an [eval] for such zone *)
      let f1 = T1.eval targets in
      let f2 = T2.eval targets in
      (fun exp man flow ->
         match f1 exp man flow with
         | Some evl -> Some evl

         | None -> f2 exp man flow
      )


  (** Query handler *)
  let ask targets =
match sat_targets ~targets ~leaves:T1.leaves,
          sat_targets ~targets ~leaves:T2.leaves
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [eval] for such zone *)
      T1.ask targets

    | false, true ->
      (* Only [D2] provides an [eval] for such zone *)
      T2.ask targets

    | true, true ->
      (* Both [D1] and [D2] provide an [eval] for such zone *)
      let f1 = T1.ask targets in
      let f2 = T2.ask targets in
      (fun q man flow ->
         OptionExt.neutral2 (join_query q) (f1 q man flow) (f2 q man flow)
      )

end



let rec make (domains:(module STATELESS) list) : (module STATELESS) =
  match domains with
  | [] -> assert false
  | [d] -> d
  | l ->
    let a,b = ListExt.split l in
    let aa, bb = make a, make b in
    (module Make(val aa : STATELESS)(val bb : STATELESS))
