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

(** Reduction operators of non-relational abstract values *)

open Core.Id


(** Manager for value reduction rules *)
type 'a value_reduction_man = {
  get : 'r. 'r id -> 'a -> 'r;
  set : 'r. 'r id -> 'r -> 'a -> 'a;
}



module type VALUE_REDUCTION =
sig
  val name   : string
  val reduce : 'v value_reduction_man -> 'v -> 'v
end

(** Registration *)
let reductions : (module VALUE_REDUCTION) list ref = ref []

let register_value_reduction rule =
  reductions := rule :: !reductions

let find_value_reduction name =
  List.find (fun v ->
      let module V = (val v : VALUE_REDUCTION) in
      compare V.name name = 0
    ) !reductions

let simplified_value_reductions () =
  List.map (fun v ->
      let module D = (val v : VALUE_REDUCTION) in
      D.name
    ) !reductions
