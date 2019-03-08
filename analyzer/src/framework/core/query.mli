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


module GenFunQuery
    (Q:
     sig
       type arg
       type ret
       val join : ret -> ret -> ret
       val meet : ret -> ret -> ret
     end)
    :
    sig
      val query : Q.arg -> Q.ret query
    end

module GenUnitQuery
    (Q:
     sig
       type ret
       val join : ret -> ret -> ret
       val meet : ret -> ret -> ret
     end)
    :
    sig
      val query : Q.ret query
    end

val join : 'a query -> 'a -> 'a -> 'a

val meet : 'a query -> 'a -> 'a -> 'a

val print_var_query : (Format.formatter -> string -> unit) query
