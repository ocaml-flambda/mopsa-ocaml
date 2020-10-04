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

(** Abstract value representation *)

open Query
open Ast.Constant
open Ast.Expr
open Ast.Typ
open Location


type _ avalue_kind = ..

type avalue_pool = {
  pool_typ : 'v. 'v avalue_kind -> typ;
  pool_bottom : 'v. 'v avalue_kind -> 'v;
  pool_top : 'v. 'v avalue_kind -> 'v;
  pool_join : 'v. 'v avalue_kind -> 'v -> 'v -> 'v;
  pool_meet : 'v. 'v avalue_kind -> 'v -> 'v -> 'v;
  pool_compare : 'v 'w. 'v avalue_kind -> 'v -> 'w avalue_kind -> 'w -> int;
  pool_print : 'v. 'v avalue_kind -> Format.formatter -> 'v -> unit;
}

type avalue_info = {
  typ : 'v. avalue_pool -> 'v avalue_kind -> typ;
  bottom : 'v. avalue_pool -> 'v avalue_kind -> 'v;
  top : 'v. avalue_pool -> 'v avalue_kind -> 'v;
  join : 'v. avalue_pool -> 'v avalue_kind -> 'v -> 'v -> 'v;
  meet : 'v. avalue_pool -> 'v avalue_kind -> 'v -> 'v -> 'v;  
  compare : 'v 'w. avalue_pool -> 'v avalue_kind -> 'v -> 'w avalue_kind -> 'w -> int;
  print : 'v. avalue_pool -> 'v avalue_kind -> Format.formatter -> 'v -> unit;
}

val register_avalue : avalue_info -> unit

val type_of_avalue : 'v avalue_kind -> typ

val bottom_avalue : 'v avalue_kind -> 'v

val top_avalue : 'v avalue_kind -> 'v

val join_avalue : 'v avalue_kind -> 'v -> 'v -> 'v

val meet_avalue : 'v avalue_kind -> 'v -> 'v -> 'v

val compare_avalue : 'v avalue_kind -> 'v -> 'w avalue_kind -> 'w -> int

val pp_avalue : 'v avalue_kind -> Format.formatter -> 'v -> unit

type constant += C_avalue : 'v avalue_kind * 'v -> constant

val mk_avalue_expr : 'v avalue_kind -> 'v -> range -> expr

val mk_avalue_constant : 'v avalue_kind -> 'v -> constant

type ('a,_) query += Q_avalue : expr * 'v avalue_kind-> ('a,'v) query

val mk_avalue_query : expr -> 'v avalue_kind -> ('a,'v) query
