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

(** Signature of reduction rules for product post-states *)

open Core.All


(*==========================================================================*)
(**                       {1 Reduction manager}                             *)
(*==========================================================================*)

(** Manager used by reduction rules *)
type 'a exec_reduction_man = {
  get_man : 't. 't id -> ('a, 't) man; (** Get the manger of a domain *)
}


(*==========================================================================*)
(**                             {1 Signature}                               *)
(*==========================================================================*)

module type EXEC_REDUCTION =
sig
  val name   : string
  (** Name of the reduction rule *)

  val reduce : stmt -> ('a,'b) man -> 'a exec_reduction_man ->
    'a flow -> 'a flow  -> 'a post option
  (** [reduce s man erman input output] reduces post-state [output] that
      resulted from executing statement [s] on pre-state [input] *)
end



(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

(** Register a new exec reduction *)
val register_exec_reduction : (module EXEC_REDUCTION) -> unit

(** Find an exec reduction by its name *)
val find_exec_reduction : string -> (module EXEC_REDUCTION)
