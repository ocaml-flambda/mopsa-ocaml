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

(** Reduction rules for abstract evaluations *)

open Core.All



(** Manager used by reduction rules *)
type 'a exec_reduction_man = {
  get_man : 't. 't id -> ('a, 't) man;
}


(** Signature of a reduction rule for post-conditions *)
module type EXEC_REDUCTION =
sig
  val name   : string
  val reduce : stmt -> ('a,'b) man -> 'a exec_reduction_man ->
    'a flow -> 'a flow  -> teffect ->
    'a post option
end


(** {2 Registration} *)
(** **************** *)

(** Registered exec reductions *)
let exec_reductions : (module EXEC_REDUCTION) list ref = ref []

(** Register a new exec reduction *)
let register_exec_reduction rule =
  exec_reductions := rule :: !exec_reductions

(** Find an exec reduction by its name *)
let find_exec_reduction name =
  List.find (fun v ->
      let module V = (val v : EXEC_REDUCTION) in
      compare V.name name = 0
    ) !exec_reductions
