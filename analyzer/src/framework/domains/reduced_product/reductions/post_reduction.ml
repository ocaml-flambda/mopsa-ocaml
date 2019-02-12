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

(** Reduction operators of post-conditions *)

open Manager
open Pool

module type REDUCTION =
sig
  val trigger : Post.channel option
  val reduce : Ast.stmt -> ('a, 'd) domain_man -> ('a, 'v) nonrel_man -> ('a, 'b) man -> 'a flow -> 'a flow
end

(** Registration *)
let reductions : (string * (module REDUCTION)) list ref = ref []
let register_reduction name rule = reductions := (name, rule) :: !reductions
let find_reduction name = List.assoc name !reductions
