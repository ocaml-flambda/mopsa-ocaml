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


(** Sequence combiner to build Cartesian products of domains *)

open Ast.All
open Core.All
open Tree


module Make
    (T1:STACKED)
    (T2:STACKED)
  : STACKED with type t = T1.t * T2.t
=
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  type t = T1.t * T2.t

  let id = I_binary(Sequence,T1.id,T2.id)

  let name = T1.name ^ " ; " ^ T2.name

  let leaves = T1.leaves @ T2.leaves

  let dependencies = T1.dependencies @ T2.dependencies

  let alarms = T1.alarms @ T2.alarms |> List.sort_uniq compare

  let bottom = T1.bottom, T2.bottom

  let top = T1.top, T2.top

  let is_bottom (a1,a2) =
    T1.is_bottom a1 ||
    T2.is_bottom a2

  let print fmt (a1,a2) =
    Format.fprintf fmt "%a%a"
      T1.print a1
      T2.print a2


  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let subset man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let b1, s, s' = T1.subset (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let b2, s, s' = T2.subset (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    b1 && b2, s, s'

  let join man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s' = T1.join (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let aa2, s, s' = T2.join (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let meet man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s' = T1.meet (fst_pair_man man) sman  ctx (a1,s) (a1',s') in
    let aa2, s, s' = T2.meet (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let widen man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s', stable1 = T1.widen (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let aa2, s, s', stable2 = T2.widen (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s', stable1 && stable2

  let merge (pre1,pre2) ((a1,a2), log) ((a1',a2'), log') =
    T1.merge pre1 (a1, Log.get_left_log log) (a1', Log.get_left_log log'),
    T2.merge pre2 (a2, Log.get_right_log log) (a2', Log.get_right_log log')



  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow =
    T1.init prog (fst_pair_man man) flow |>
    T2.init prog (snd_pair_man man)

  (** Execution of statements *)
  let exec targets =
    match sat_targets ~targets ~leaves:T1.leaves,
          sat_targets ~targets ~leaves:T2.leaves
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such targets *)
      raise Not_found

    | true, false ->
      (* Only [S1] provides an [exec] for such targets *)
      let f = T1.exec targets in
      (fun stmt man flow ->
         f stmt (fst_pair_man man) flow)

    | false, true ->
      (* Only [S2] provides an [exec] for such targets *)
      let f = T2.exec targets in
      (fun stmt man flow ->
         f stmt (snd_pair_man man) flow)

    | true, true ->
      (* Both [S1] and [S2] provide an [exec] for such targets *)
      let f1 = T1.exec targets in
      let f2 = T2.exec targets in
      (fun stmt man flow ->
         match f1 stmt (fst_pair_man man) flow with
         | Some post ->
           OptionExt.return post

         | None ->
           f2 stmt (snd_pair_man man) flow)


  (** Evaluation of expressions *)
  let eval targets =
    match sat_targets ~targets ~leaves:T1.leaves,
          sat_targets ~targets ~leaves:T2.leaves
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such targets *)
      raise Not_found

    | true, false ->
      (* Only [S1] provides an [eval] for such targets *)
      let f = T1.eval targets in
      (fun exp man flow ->
         f exp (fst_pair_man man) flow)

    | false, true ->
      (* Only [S2] provides an [eval] for such targets *)
      let f = T2.eval targets in
      (fun exp man flow ->
         f exp (snd_pair_man man) flow)

    | true, true ->
      (* Both [S1] and [S2] provide an [eval] for such targets *)
      let f1 = T1.eval targets in
      let f2 = T2.eval targets in
      (fun exp man flow ->
         match f1 exp (fst_pair_man man) flow with
         | Some evl -> Some evl

         | None -> f2 exp (snd_pair_man man) flow)

  (** Query handler *)
  let ask targets =
    match sat_targets ~targets ~leaves:T1.leaves,
          sat_targets ~targets ~leaves:T2.leaves
    with
    | false, false -> raise Not_found

    | true, false ->
      let f = T1.ask targets in
      (fun q man flow ->
         f q (fst_pair_man man) flow)

    | false, true ->
      let f = T2.ask targets in
      (fun q man flow ->
         f q (snd_pair_man man) flow)

    | true, true ->
      let f1 = T1.ask targets in
      let f2 = T2.ask targets in
      (fun q man flow ->
         let reply1 = f1 q (fst_pair_man man) flow in
         let reply2 = f2 q (snd_pair_man man) flow in
         OptionExt.neutral2 (join_query q) reply1 reply2)

end


let rec make (domains:(module STACKED) list) : (module STACKED) =
  match domains with
  | [] -> assert false
  | [d] -> d
  | l ->
    let a,b = ListExt.split l in
    let aa, bb = make a, make b in
    (module Make(val aa : STACKED)(val bb : STACKED))
