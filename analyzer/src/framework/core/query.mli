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

(** Generic query mechanism for extracting information from domains. *)


type _ query

val join : 'a query -> 'a -> 'a -> 'a

val meet : 'a query -> 'a -> 'a -> 'a

module type UnitQuery =
sig
  type ret
  val query : ret query
  val handle : 'r query -> (unit -> ret) -> 'r option
end

module GenUnitQuery
    (Spec: sig
       type ret
       val join : ret -> ret -> ret
       val meet : ret -> ret -> ret
     end)
  : UnitQuery
    with type ret = Spec.ret

module type ArgQuery =
sig
  type ret
  type arg
  val query : arg -> ret query
  val handle : 'r query -> (arg -> ret) -> 'r option
end


module GenArgQuery
    (Spec: sig
       type arg
       type ret
       val join : ret -> ret -> ret
       val meet : ret -> ret -> ret
     end)
  : ArgQuery
    with type ret = Spec.ret
    with type arg = Spec.arg

module PrintVarQuery :
sig
  val query   : (Format.formatter -> string -> unit) query
  val handle : 'r query -> (unit -> (Format.formatter -> string -> unit)) -> 'r option
end

