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


(** Generators of identifiers for domains and values *)

open Eq


type _ id = ..
(** Identifiers *)


type witness = {
  eq :  'a 'b. 'a id -> 'b id -> ('a,'b) eq option;
}

type witness_chain = {
  eq :  'a 'b. witness -> 'a id -> 'b id -> ('a,'b) eq option;
}
(** Equality witness of an identifier *)


val register_id : witness_chain -> unit
(** Register a new descriptor *)


val equal_id : 'a id -> 'b id -> ('a,'b) eq option
(** Equality witness of identifiers *)




(** Generator of a new domain identifier *)
module GenDomainId(
    Spec: sig
      type t
      val name : string
    end
  ) :
sig
  val id : Spec.t id
  val name : string
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
end


(** Generator of a new identifier for stateless domains *)
module GenStatelessDomainId(
    Spec: sig
      val name : string
    end
  ) :
sig
  val id : unit id
  val name : string
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
end



(** Generator of a new value identifier *)
module GenValueId(
    Spec:sig
      type t
      val name : string
      val display : string
    end
  ) :
sig
  val id : Spec.t id
  val name : string
  val display : string
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
end
