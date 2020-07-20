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

(** Manager used by simplified reduction rules *)
type ('a,'b) simplified_reduction_man = {
  get_env : 't. 't id -> 'b -> 't;
  set_env : 't. 't id -> 't -> 'b -> 'b;
  get_value : 't. 't id -> var -> 'b -> 't;
  set_value : 't. 't id -> var -> 't -> 'b -> 'b;
  ask : 'r. ('a,'r) query -> uctx -> 'b -> 'r;
}



(** Signature of simplified reduction rules *)
module type SIMPLIFIED_REDUCTION =
sig
  val name   : string
  val reduce : stmt -> ('a,'b) simplified_reduction_man -> uctx -> 'b -> 'b -> 'b
end


(** {2 Registration} *)
(** **************** *)

(** Registered simplified reductions *)
let simplified_reductions : (module SIMPLIFIED_REDUCTION) list ref = ref []


(** Register a new simplified reduction *)
let register_simplified_reduction rule =
  simplified_reductions := rule :: !simplified_reductions

(** Find an simplified reduction by its name *)
let find_simplified_reduction name =
  List.find (fun v ->
      let module V = (val v : SIMPLIFIED_REDUCTION) in
      compare V.name name = 0
    ) !simplified_reductions
