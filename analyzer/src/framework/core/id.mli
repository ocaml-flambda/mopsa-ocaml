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


type _ domain = ..
(** Domain identifier *)


type 'a domain_info = {
  eq :  'b. 'b domain -> ('b,'a) eq option;
}
(** Descriptor of a domain identifier *)


val register_domain_id : 'a domain_info -> unit
(** Register a new descriptor *)


val domain_id_eq : 'a domain -> 'b domain -> ('a,'b) eq option
(** Equality witness of domain identifiers *)


(** Generator of a new domain identifier *)
module GenDomainId(
    Spec: sig
      type t
      val name : string
    end
  ) :
sig
  val id : Spec.t domain
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
  val id : unit domain
  val name : string
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
end


(****************************************************************************)
(**                         {2 Value identifiers}                           *)
(****************************************************************************)


type _ value = ..
(** Value identifier *)

type 'a value_info = {
  eq :  'b. 'b value -> ('b,'a) eq option;
}
(** Descriptor of a value identifier *)


val register_value_id : 'a value_info -> unit
(** Register a new descriptor *)


val value_id_eq : 'a value -> 'b value -> ('a,'b) eq option
(** Equality witness of value identifiers *)


val value_id_eq : 'a value -> 'b value -> ('a,'b) eq option
(** Equality witness of value identifiers *)

(** Generator of a new value identifier *)
module GenValueId(
    Spec:sig
      type t
      val name : string
      val display : string
    end
  ) :
sig
  val id : Spec.t value
  val name : string
  val display : string
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
end
