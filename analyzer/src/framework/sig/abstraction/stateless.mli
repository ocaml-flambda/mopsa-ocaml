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

(** Signature of stateless domains

    The signature [STATELESS] represents domains without an abstract element.
    This can be useful for implementing iterators for example.
*)

open Core.All


(*==========================================================================*)
(**                             {1 Signature}                               *)
(*==========================================================================*)


module type STATELESS =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  val name : string
  (** Name of the domain *)

  val id : unit id
  (** Identifier of the domain *)

  val dependencies : semantic list
  (** Semantic dependencies of the domain *)

  val alarms : alarm_class list
  (** List of alarms detected by the domain *)


  (** {2 Transfer functions} *)
  (** ********************** *)
  val init : program -> ('a, unit) man -> 'a flow -> 'a flow
  (** Initialization routine *)

  val exec : stmt -> ('a, unit) man -> 'a flow -> 'a post option
  (** Computation of post-conditions *)

  val eval : expr -> ('a, unit) man -> 'a flow -> 'a rewrite option
  (** Evaluation of expressions *)

  val ask  : ('a,'r) query -> ('a, unit) man -> 'a flow -> 'r option
  (** Handler of queries *)

end


(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

val register_stateless_domain : (module STATELESS) -> unit
(** Register a new stateless domain *)


val find_stateless_domain : string -> (module STATELESS)
(** Find a stateless domain by its name. Raise [Not_found] if no domain is found *)

val mem_stateless_domain : string -> bool
(** [mem_stateless_domain name] checks whether a stateless domain with name
    [name] is registered *)
 
val stateless_domain_names : unit -> string list
(** Return the names of registered stateless domains *) 
