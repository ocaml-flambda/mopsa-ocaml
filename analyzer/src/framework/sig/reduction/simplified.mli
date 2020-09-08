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

(** Reduction rules for reduced products of simplified domains

    When a reduced product contains only domains with the simplified signature,
    reductions rules operate on the product of abstract environments instead
    of flows. 

    This simplifies the formulation of reductions and gives direct
    access to the non-relational abstraction (if any).
 *)

open Core.All


(*==========================================================================*)
(**                       {1 Reduction manager}                             *)
(*==========================================================================*)

(** Manager used by simplified reduction rules *)
type ('a,'b) simplified_reduction_man = {
  get_env : 't. 't id -> 'b -> 't;
  (** Get the environment of some domain *)

  set_env : 't. 't id -> 't -> 'b -> 'b;
  (** Set the environment of some domain *)

  get_value : 't. 't id -> var -> 'b -> 't;
  (** Get the value of a variable in some value abstraction *)

  set_value : 't. 't id -> var -> 't -> 'b -> 'b;
  (** Set the value of a variable in some value abstraction *)

  ask : 'r. ('a,'r) query -> 'a ctx -> 'b -> 'r;
  (** Perform a query *)
}


(*==========================================================================*)
(**                             {1 Signature}                               *)
(*==========================================================================*)

module type SIMPLIFIED_REDUCTION =
sig
  val name   : string
  (** Name of the reduction rule *)

  val reduce : stmt -> ('a,'b) simplified_reduction_man -> 'a ctx -> 'b -> 'b -> 'b
  (** [reduce s man ctx input output] applies a reduction rule on post-state
      [output] that resulted from executing statement [s] on pre-state [input] *)
end


(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

val register_simplified_reduction : (module SIMPLIFIED_REDUCTION) -> unit
(** Register a new simplified reduction *)

val find_simplified_reduction : string -> (module SIMPLIFIED_REDUCTION)
(** Find an simplified reduction by its name *)
