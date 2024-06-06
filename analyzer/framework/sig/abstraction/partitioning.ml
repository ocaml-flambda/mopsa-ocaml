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

(** Signature of partitioning domains.
 *
 * A partitioning domain lifts a domin D into a map K -> D, where K is the set
 * of partitioning keys. K should verify the following conditions:
 *  - finite,
 *  - defines a total order and
 *  - there is no intersection between every pair of distinct keys.
 *
 *  For the moment, we don't support modifying the partitioning keys during
 *  lattice operation (join, meet and widening)
 *)

open Core.All
open Mopsa_utils.Location

module type PARTITIONING =
sig
  type t

  val name : string
  val checks : check list

  val print : Format.formatter -> t -> unit
  val compare : t -> t -> int

  val init : t
  val add  : marker -> t -> t
  val exec : stmt -> ('a, t) man -> 'a flow -> 'a post option
  val eval : expr -> ('a, t) man -> 'a flow -> 'a eval option
  val ask  : ('a, 'r) query -> ('a, t) man -> 'a flow -> ('a, 'r) cases option
end

let domains : (module PARTITIONING) list ref = ref []

let register_partitioning dom =
  domains := dom :: !domains

let find_partitioning name =
  List.find (fun dom ->
      let module D = (val dom : PARTITIONING) in
      compare D.name name = 0
    ) !domains

let mem_partitioning name =
  List.exists (fun dom ->
      let module D = (val dom : PARTITIONING) in
      compare D.name name = 0
    ) !domains
 
let partitioning_names () =
  List.map (fun dom ->
      let module D = (val dom : PARTITIONING) in
      D.name
    ) !domains

type ('a, _) query += Q_partition_predicate : range -> ('a, expr) query

let () = register_query {
    join = (let doit : type a r. query_pool -> (a, r) query -> r -> r -> r = fun pool query x y ->
        match query with
        | Q_partition_predicate range ->
          mk_binop x O_log_or y ~etyp:T_bool range
        | _ -> pool.pool_join query x y
       in doit
      );
    meet = (let doit : type a r. query_pool -> (a, r) query -> r -> r -> r = fun pool query x y ->
        match query with
        | Q_partition_predicate range ->
          mk_binop x O_log_and y ~etyp:T_bool range
        | _ -> pool.pool_meet query x y
       in doit
      );
  }
