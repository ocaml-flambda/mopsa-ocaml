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


type _ did
(** Domain identifier *)


val deq : 'a did -> 'b did -> ('a,'b) eq option
(** Equality witness of domain identifiers *)

(** Generator of a new domain identifier *)
module GenDomainId(
    Spec: sig
      type typ
      val name : string
    end
  ) :
sig
  val id : Spec.typ did
  val name : string
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
end


(****************************************************************************)
(**                         {2 Value identifiers}                           *)
(****************************************************************************)


type _ vid
(** Value identifier *)


val veq : 'a vid -> 'b vid -> ('a,'b) eq option
(** Equality witness of value identifiers *)

(** Generator of a new value identifier *)
module GenValueId(
    Spec:sig
      type typ
      val name : string
      val display : string
    end
  ) :
sig
  val id : Spec.typ vid
  val name : string
  val display : string
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
end

