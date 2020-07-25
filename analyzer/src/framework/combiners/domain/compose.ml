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

(** Composition combiner to stack a domain over another one *)


open Core.All
open Sig.Combiner.Stacked
open Common

module Make
    (C1:STACKED_COMBINER)
    (C2:STACKED_COMBINER)
  : STACKED_COMBINER with type t = C1.t * C2.t
=
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  type t = C1.t * C2.t

  let id = C_pair(Compose,C1.id,C2.id)

  let name = C1.name ^ " o " ^ C2.name

  let domains = C1.domains @ C2.domains

  let semantics = C1.semantics @ C2.semantics

  let routing_table =
    let t1 = List.fold_left
        (fun acc d1 -> add_routes (BelowOf d1) C2.domains acc)
        (join_routing_table C1.routing_table C2.routing_table)
        C1.domains
    in
    let t2 = List.fold_left
        (fun acc s1 -> add_routes (Semantic s1) C2.domains acc)
        t1
        C1.semantics
    in
    t2

  let alarms = C1.alarms @ C2.alarms |> List.sort_uniq compare

  let bottom = C1.bottom, C2.bottom

  let top = C1.top, C2.top

  let is_bottom (a1,a2) =
    C1.is_bottom a1 ||
    C2.is_bottom a2

  let print fmt (a1,a2) =
    Format.fprintf fmt "%a%a"
      C1.print a1
      C2.print a2

  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let fst_pair_sman man sman = {
    get_sub = (fun a -> get_pair_snd man a, sman.get_sub a);
    set_sub = (fun (a2,s) a -> set_pair_snd man a2 a |> sman.set_sub s);
  }

  let subset man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let b1, (a2,s), (a2',s') = C1.subset (fst_pair_man man) (fst_pair_sman man sman) ctx (a1,(a2,s)) (a1',(a2',s')) in
    let b2, s, s' = C2.subset (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    b1 && b2, s, s'

  let join man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, (a2,s), (a2',s') = C1.join (fst_pair_man man) (fst_pair_sman man sman) ctx (a1,(a2,s)) (a1',(a2',s')) in
    let aa2, s, s' = C2.join (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let meet man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, (a2,s), (a2',s') = C1.meet (fst_pair_man man) (fst_pair_sman man sman) ctx (a1,(a2,s)) (a1',(a2',s')) in
    let aa2, s, s' = C2.meet (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s'

  let widen man sman ctx ((a1,a2),s) ((a1',a2'),s') =
    let aa1, (a2,s), (a2',s'), stable1 = C1.widen (fst_pair_man man) (fst_pair_sman man sman) ctx (a1,(a2,s)) (a1',(a2',s')) in
    let aa2, s, s', stable2 = C2.widen (snd_pair_man man) sman ctx (a2,s) (a2',s') in
    (aa1,aa2), s, s', stable1 && stable2

  let merge (pre1,pre2) ((a1,a2), log) ((a1',a2'), log') =
    C1.merge pre1 (a1, Log.get_left_log log) (a1', Log.get_left_log log'),
    C2.merge pre2 (a2, Log.get_right_log log) (a2', Log.get_right_log log')



  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow =
    C1.init prog (fst_pair_man man) flow |>
    C2.init prog (snd_pair_man man)

  (** Execution of statements *)
  let exec targets =
    match sat_targets ~targets ~domains:C1.domains,
          sat_targets ~targets ~domains:C2.domains
    with
    | false, false ->
      (* Both operands do not provide an [exec] for such targets *)
      raise Not_found

    | true, false ->
      (* Only [T1] provides an [exec] for such targets *)
      let f = C1.exec targets in
      (fun stmt man flow ->
         f stmt (fst_pair_man man) flow
      )

    | false, true ->
      (* Only [T2] provides an [exec] for such targets *)
      let f = C2.exec targets in
      (fun stmt man flow ->
         f stmt (snd_pair_man man) flow
      )

    | true, true ->
      (* Both [T1] and [T2] provide an [exec] for such targets *)
      let f1 = C1.exec targets in
      let f2 = C2.exec targets in
      (fun stmt man flow ->
         match f1 stmt (fst_pair_man man) flow with
         | Some post ->
           OptionExt.return post

         | None ->
           f2 stmt (snd_pair_man man) flow
      )


  (** Evaluation of expressions *)
  let eval targets =
    match sat_targets ~targets ~domains:C1.domains,
          sat_targets ~targets ~domains:C2.domains
    with
    | false, false ->
      (* Both operands do not provide an [eval] for such targets *)
      raise Not_found

    | true, false ->
      (* Only [T1] provides an [eval] for such targets *)
      let f = C1.eval targets in
      (fun exp man flow ->
         f exp (fst_pair_man man) flow
      )

    | false, true ->
      (* Only [T2] provides an [eval] for such targets *)
      let f = C2.eval targets in
      (fun exp man flow ->
         f exp (snd_pair_man man) flow
      )

    | true, true ->
      (* Both [T1] and [T2] provide an [eval] for such targets *)
      let f1 = C1.eval targets in
      let f2 = C2.eval targets in
      (fun exp man flow ->
         match f1 exp (fst_pair_man man) flow with
         | Some evl -> Some evl

         | None -> f2 exp (snd_pair_man man) flow
      )


  (** Query handler *)
  let ask targets =
    match sat_targets ~targets ~domains:C1.domains,
          sat_targets ~targets ~domains:C2.domains
    with
    | false, false -> raise Not_found

    | true, false ->
      let f = C1.ask targets in
      (fun q man flow ->
         f q (fst_pair_man man) flow
      )

    | false, true ->
      let f = C2.ask targets in
      (fun q man flow ->
         f q (snd_pair_man man) flow
      )

    | true, true ->
      let f1 = C1.ask targets in
      let f2 = C2.ask targets in
      (fun q man flow ->
         OptionExt.neutral2
           (join_query q ~join:(fun a b -> man.lattice.join (Flow.get_unit_ctx flow) a b))
           (f1 q (fst_pair_man man) flow)
           (f2 q (snd_pair_man man) flow)
      )
    


end


let rec make (domains:(module STACKED_COMBINER) list) : (module STACKED_COMBINER) =
  match domains with
  | [] -> assert false
  | [d] -> d
  | l ->
    let a,b = ListExt.split l in
    let aa, bb = make a, make b in
    (module Make(val aa : STACKED_COMBINER)(val bb : STACKED_COMBINER))
