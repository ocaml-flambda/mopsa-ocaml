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

(** Common constructs for points-to evaluations. *)

open Mopsa
open Universal.Ast
open Base
open Alarms

(* Points-to records *)
(* ================= *)

type points_to =
  | P_fun of Ast.c_fundec
  | P_block of base (** base *) * expr (** offset *) * mode option (** strong or weak *)
  | P_null
  | P_invalid
  | P_top

let mk_c_points_to_bloc b o m = P_block (b, o, m)

let mk_c_points_to_null = P_null

let mk_c_points_to_invalid = P_invalid

let mk_c_points_to_fun f = P_fun f

let mk_c_points_to_top = P_top

let pp_points_to fmt = function
  | P_fun f -> Format.fprintf fmt "(fp %s)" f.Ast.c_func_org_name
  | P_block(base, offset, None) -> Format.fprintf fmt "(%a, %a)" pp_base base pp_expr offset
  | P_block(base, offset, Some mode) -> Format.fprintf fmt "(%a, %a, %a)" pp_base base pp_expr offset pp_mode mode
  | P_null -> Format.pp_print_string fmt "NULL"
  | P_invalid -> Format.pp_print_string fmt "Invalid"
  | P_top -> Format.pp_print_string fmt "⊤"

let compare_points_to p1 p2 =
  match p1, p2 with
  | P_fun f1, P_fun f2 ->
    compare f1.Ast.c_func_unique_name f2.Ast.c_func_unique_name
  | P_block (b1, o1, m1), P_block (b2, o2, m2) ->
    Compare.compose [
      (fun () -> compare_base b1 b2);
      (fun () -> compare_expr o1 o2);
      (fun () -> Option.compare compare_mode m1 m2);
    ]
  | _, _ -> Stdlib.compare p1 p2


(* Points-to query *)
(* =============== *)

type ('a,_) query += Q_c_points_to : expr -> ('a,('a,points_to) cases) query

let () = register_query {
    join = (
      let f : type a r. query_pool -> (a,r) query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_c_points_to _ -> Cases.join a b
          | _ -> next.pool_join query a b
      in
      f
    );
    meet = (
      let f : type a r. query_pool -> (a,r) query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_c_points_to _ -> Cases.meet a b
          | _ -> next.pool_meet query a b
      in
      f
    );
  }


let resolve_pointer p man flow = man.ask (Q_c_points_to p) flow


(* Points-to containers *)
(* ==================== *)

module PointsToSet = SetExt.Make(struct type t = points_to let compare = compare_points_to end)
module PointsToMap = MapExt.Make(struct type t = points_to let compare = compare_points_to end)
