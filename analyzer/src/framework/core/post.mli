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

(** Post-states of statement transfer functions *)

open Ast
open Lattice
open Flow
open Eval



(****************************************************************************)
(**                          {2 Post-states}                                *)
(****************************************************************************)

(** Combiner merge logs *)
type clog =
   | L_leaf
   | L_product of clog list
   | L_compose of clog * log

(** Merge logs *)
and log = Ast.stmt list * clog

(** Post-state *)
type 'a post

val of_flow : 'a flow -> 'a post
(** [of_flow flow] returns a post-condition from a flow, without merge logs *)

val to_flow : 'a lattice -> 'a post -> 'a flow

val return_flow : 'a flow -> 'a post option

val map_flow : ('a flow -> 'a flow) -> 'a post -> 'a post
(** [map_flow f p] applies [f] on the underlying flow in [p]*)

val print : 'a lattice -> Format.formatter -> 'a post -> unit

val bind_eval : ('e -> 'a flow -> 'a post) -> ('e, 'a) eval -> 'a post
