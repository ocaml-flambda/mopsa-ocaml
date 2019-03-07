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

(** Journal logs *)

open Ast.Stmt

type log =
  | L_empty
  | L_domain of block * log
  | L_compound of log list

let rec concat log1 log2 =
  match log1, log2 with
  | L_empty, x | x, L_empty -> x
  | L_domain (b1, l1), L_domain (b2, l2) -> L_domain (b1 @ b2, concat l1 l2)
  | L_compound ll1, L_compound ll2 -> L_compound (List.map2 concat ll1 ll2)
  | _ -> assert false

let empty = L_empty

let is_empty log =
  match log with
  | L_empty -> true
  | _ -> false

let get_domain_block log =
  match log with
  | L_domain(block, _) -> block
  | _ -> assert false

let get_domain_log log =
  match log with
  | L_domain(_, l) -> l
  | _ -> assert false

