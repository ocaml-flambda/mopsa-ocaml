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


(** Switch combiner *)

open Core.All
open Sig.Combiner.Stacked
open Common


module Make
    (D1:STACKED_COMBINER)
    (D2:STACKED_COMBINER)
  : STACKED_COMBINER with type t = D1.t * D2.t
=
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  type t = D1.t * D2.t

  let id = C_pair(Sequence,D1.id,D2.id)

  let name = D1.name ^ " ; " ^ D2.name

  let domains = D1.domains @ D2.domains

  let semantics = D1.semantics @ D2.semantics

  let routing_table =
    let t1 = List.fold_left
        (fun acc d1 -> add_routes (Below d1) D2.domains acc)
        (join_routing_table D1.routing_table D2.routing_table)
        D1.domains
    in
    let t2 = List.fold_left
        (fun acc s1 -> add_routes (Semantic s1) D2.domains acc)
        t1
        D1.semantics
    in
    t2


  let alarms = D1.alarms @ D2.alarms |> List.sort_uniq compare

  let bottom = D1.bottom, D2.bottom

  let top = D1.top, D2.top

  let is_bottom (a1,a2) =
    D1.is_bottom a1 ||
    D2.is_bottom a2

  let print fmt (a1,a2) =
    Format.fprintf fmt "%a%a"
      D1.print a1
      D2.print a2


  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let subset man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let b1, s, s' = D1.subset (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let b2, s, s' = D2.subset (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    b1 && b2, s, s'

  let join man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s' = D1.join (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let aa2, s, s' = D2.join (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let meet man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s' = D1.meet (fst_pair_man man) sman  ctx (a1,s) (a1',s') in
    let aa2, s, s' = D2.meet (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let widen man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, s, s', stable1 = D1.widen (fst_pair_man man) sman ctx (a1,s) (a1',s') in
    let aa2, s, s', stable2 = D2.widen (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s', stable1 && stable2

  let merge (pre1,pre2) ((a1,a2), log) ((a1',a2'), log') =
    D1.merge pre1 (a1, Log.get_left_log log) (a1', Log.get_left_log log'),
    D2.merge pre2 (a2, Log.get_right_log log) (a2', Log.get_right_log log')



  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow =
    D1.init prog (fst_pair_man man) flow |>
    D2.init prog (snd_pair_man man)

  (** Execution of statements *)
  let exec targets =
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D2.domains
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such targets *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [exec] for such targets *)
      let f = D1.exec targets in
      (fun stmt man flow ->
         f stmt (fst_pair_man man) flow)

    | false, true ->
      (* Only [D2] provides an [exec] for such targets *)
      let f = D2.exec targets in
      (fun stmt man flow ->
         f stmt (snd_pair_man man) flow)

    | true, true ->
      (* Both [D1] and [D2] provide an [exec] for such targets *)
      let f1 = D1.exec targets in
      let f2 = D2.exec targets in
      (fun stmt man flow ->
         match f1 stmt (fst_pair_man man) flow with
         | Some post ->
           OptionExt.return post

         | None ->
           f2 stmt (snd_pair_man man) flow)


  (** Evaluation of expressions *)
  let eval targets =
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D2.domains
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such targets *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [eval] for such targets *)
      let f = D1.eval targets in
      (fun exp man flow ->
         f exp (fst_pair_man man) flow)

    | false, true ->
      (* Only [D2] provides an [eval] for such targets *)
      let f = D2.eval targets in
      (fun exp man flow ->
         f exp (snd_pair_man man) flow)

    | true, true ->
      (* Both [D1] and [D2] provide an [eval] for such targets *)
      let f1 = D1.eval targets in
      let f2 = D2.eval targets in
      (fun exp man flow ->
         match f1 exp (fst_pair_man man) flow with
         | Some evl -> Some evl

         | None -> f2 exp (snd_pair_man man) flow)

  (** Query handler *)
  let ask targets =
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D2.domains
    with
    | false, false -> raise Not_found

    | true, false ->
      let f = D1.ask targets in
      (fun q man flow ->
         f q (fst_pair_man man) flow)

    | false, true ->
      let f = D2.ask targets in
      (fun q man flow ->
         f q (snd_pair_man man) flow)

    | true, true ->
      let f1 = D1.ask targets in
      let f2 = D2.ask targets in
      (fun q man flow ->
         OptionExt.neutral2
           (join_query q ~join:(fun a b -> man.lattice.join (Flow.get_unit_ctx flow) a b))
           (f1 q (fst_pair_man man) flow)
           (f2 q (snd_pair_man man) flow))
end


let rec make (domains:(module STACKED_COMBINER) list) : (module STACKED_COMBINER) =
  match domains with
  | [] -> assert false
  | [d] -> d
  | l ->
    let a,b = ListExt.split l in
    let aa, bb = make a, make b in
    (module Make(val aa : STACKED_COMBINER)(val bb : STACKED_COMBINER))
