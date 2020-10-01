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
open Ast.Expr

type _ aval = ..

val bottom_aval : 'r aval -> 'r

val top_aval : 'r aval -> 'r

val join_aval : 'r aval -> 'r -> 'r -> 'r

val meet_aval : 'r aval -> 'r -> 'r -> 'r

type ('a,_) query += Q_expr_aval : expr * 'r aval-> ('a,'r) query

val mk_expr_aval_query : expr -> 'r aval -> ('a,'r) query
