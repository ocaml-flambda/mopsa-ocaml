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


(** Apply combiner *)

open Core.All
open Sig.Combiner.Stacked
open Sig.Combiner.Domain
open Common
open Mopsa_utils


module Make
    (D1:STACKED_COMBINER)
    (D2:DOMAIN_COMBINER)
  : DOMAIN_COMBINER with type t = D1.t * D2.t
=
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  type t = D1.t * D2.t

  let id = C_pair(Sequence,D1.id,D2.id)

  let name = D1.name ^ "(" ^ D2.name ^ ")"

  let domains = DomainSet.union D1.domains D2.domains

  let semantics = SemanticSet.union D1.semantics D2.semantics

  let routing_table =
    let t = join_routing_table D1.routing_table D2.routing_table in
    DomainSet.fold
      (fun d1 acc -> add_routes (Below d1) D2.domains acc)
      D1.domains t

  let checks = D1.checks @ D2.checks |> List.sort_uniq compare

  let bottom = D1.bottom, D2.bottom

  let top = D1.top, D2.top

  let is_bottom (a1,a2) =
    D1.is_bottom a1 ||
    D2.is_bottom a2


  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let subset man ctx ((a1,a2), s) ((a1',a2'), s') =
    let b1, ss, ss' = D1.subset (fst_pair_man man) ctx (a1, s) (a1', s') in
    b1 && (
      let a2 = if s == ss then a2 else get_singleton_env ctx man ss |> snd in
      let a2' = if s' == ss' then a2' else get_singleton_env ctx man ss' |> snd in
      D2.subset (snd_pair_man man) ctx (a2, ss) (a2', ss')
    )

  let join man ctx ((a1,a2), s) ((a1',a2'), s') =
    let aa, ss, ss' = D1.join (fst_pair_man man) ctx (a1, s) (a1', s') in
    let a2 = if s == ss then a2 else get_singleton_env ctx man ss |> snd in
    let a2' = if s' == ss' then a2' else get_singleton_env ctx man ss' |> snd in
    let aa' = D2.join (snd_pair_man man) ctx (a2, ss) (a2', ss') in
    aa, aa'

  let meet man ctx ((a1,a2), s) ((a1',a2'), s') =
    let aa, ss, ss' = D1.meet (fst_pair_man man) ctx (a1, s) (a1', s') in
    let a2 = if s == ss then a2 else get_singleton_env ctx man ss |> snd in
    let a2' = if s' == ss' then a2' else get_singleton_env ctx man ss' |> snd in
    let aa' = D2.meet (snd_pair_man man) ctx (a2, ss) (a2', ss') in
    aa, aa'

  let widen man ctx ((a1,a2), s) ((a1',a2'), s') =
    let aa, ss, ss',_ = D1.widen (fst_pair_man man) ctx (a1, s) (a1', s') in
    let a2 = if s == ss then a2 else get_singleton_env ctx man ss |> snd in
    let a2' = if s' == ss' then a2' else get_singleton_env ctx man ss' |> snd in
    let aa' = D2.widen (snd_pair_man man) ctx (a2, ss) (a2', ss') in
    aa, aa'

  let merge path (pre1,pre2) ((a1,a2), te) ((a1',a2'), te') =
    D1.merge (Ax_pair_left::path) pre1 (a1, te) (a1', te'),
    D2.merge (Ax_pair_right::path) pre2 (a2, te) (a2', te')



  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow = broadcast_init D1.init D2.init prog man flow
  
  (** Execution of statements *)
  let exec targets = cascade_call targets D1.exec D1.domains D2.exec D2.domains


  (** Evaluation of expressions *)
  let eval targets = cascade_call targets D1.eval D1.domains D2.eval D2.domains

  (** Query handler *)
  let ask targets = broadcast_call targets D1.ask D1.domains D2.ask D2.domains


  (** Pretty printer of states *)
  let print_state targets =
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D2.domains
    with
    | false, false -> raise Not_found

    | true, false ->
      let f = D1.print_state targets in
      (fun printer (a1,_) ->
         f printer a1)

    | false, true ->
      let f = D2.print_state targets in
      (fun printer (_,a2) ->
         f printer a2)

    | true, true ->
      let f1 = D1.print_state targets in
      let f2 = D2.print_state targets in
      (fun printer (a1,a2) ->
         f1 printer a1;
         f2 printer a2
      )


  (** Pretty printer of expressions *)
  let print_expr targets =
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D2.domains
    with
    | false, false -> raise Not_found

    | true, false ->
      let f = D1.print_expr targets in
      (fun man flow printer e ->
         f (fst_pair_man man) flow printer e)

    | false, true ->
      let f = D2.print_expr targets in
      (fun man flow printer e ->
         f (snd_pair_man man) flow printer e)

    | true, true ->
      let f1 = D1.print_expr targets in
      let f2 = D2.print_expr targets in
      (fun man flow printer e ->
         f1 (fst_pair_man man) flow printer e;
         f2 (snd_pair_man man) flow printer e
      )

end
