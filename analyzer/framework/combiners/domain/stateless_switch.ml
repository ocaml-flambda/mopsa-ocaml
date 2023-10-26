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

(** Switch of stateless domains *)

open Mopsa_utils
open Core.All
open Sig.Combiner.Stateless
open Common


module Make(D1:STATELESS_COMBINER)(D2:STATELESS_COMBINER) : STATELESS_COMBINER =
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  let id = C_empty

  let name = D1.name ^ " ; " ^ D2.name

  let domains = DomainSet.union D1.domains D2.domains

  let semantics = SemanticSet.union D1.semantics D2.semantics

  let routing_table =
    let t = join_routing_table D1.routing_table D2.routing_table in
    DomainSet.fold
      (fun d1 acc -> add_routes (Below d1) D2.domains acc)
      D1.domains t

  let checks = D1.checks @ D2.checks |> List.sort_uniq compare

  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow =
    D1.init prog man flow |>
    D2.init prog man

  (** Execution of statements *)
  let exec targets = cascade_stateless_call targets D1.exec D1.domains D2.exec D2.domains

  (** Evaluation of expressions *)
  let eval targets = cascade_stateless_call targets D1.eval D1.domains D2.eval D2.domains

  (** Query handler *)
  let ask targets = broadcast_stateless_call targets D1.ask D1.domains D2.ask D2.domains

  (** Pretty printer of expressions *)
  let print_expr targets =
    match sat_targets ~targets ~domains:D1.domains,
          sat_targets ~targets ~domains:D2.domains
    with
    | false, false -> raise Not_found

    | true, false ->
      D1.print_expr targets

    | false, true ->
      D2.print_expr targets

    | true, true ->
      let f1 = D1.print_expr targets in
      let f2 = D2.print_expr targets in
      (fun man flow printer e ->
         f1 man flow printer e;
         f2 man flow printer e
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
