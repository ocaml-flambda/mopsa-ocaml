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

(** Reduction rules of statements post-states *)

open Id
open Query
open Channel



(** Manager used by post-state reduction rules *)
type 'a xrman = {
  get : 'r. 'r domain -> 'a -> 'r;
  set : 'r. 'r domain -> 'r -> 'a -> 'a;
  ask : 'r. 'r query -> 'a -> 'r;
  refine : channel -> 'a -> 'a;
}



(** Signature of a post-state reduction rule *)
module type REDUCTION =
sig
  val name   : string
  val reduce : 'a xrman -> 'a -> 'a
end


(** Registered reductions *)
let reductions : (module REDUCTION) list ref = ref []


(** Register a new reduction *)
let register_reduction rule =
  reductions := rule :: !reductions

(** Find a reduction by its name *)
let find_reduction name =
  List.find (fun v ->
      let module V = (val v : REDUCTION) in
      compare V.name name = 0
    ) !reductions
