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

(** Flow-insensitive context *)


(****************************************************************************)
(**                          {2 Unit contexts}                              *)
(****************************************************************************)

type _ ukey

type uctx

module GenUnitKey
    (V:sig
       type t
       val print : Format.formatter -> t -> unit
     end)
  :
  sig
    val key : V.t ukey
  end

val ufind :'v ukey -> uctx -> 'v

val uempty : uctx

val uadd :'v ukey -> 'v -> uctx -> uctx

val umem : 'v ukey -> uctx -> bool

val uremove : 'v ukey -> uctx -> uctx

val uprint : Format.formatter -> uctx -> unit


(****************************************************************************)
(**                     {2 Polymorphic contexts}                            *)
(****************************************************************************)

type ('a, _) pkey

type 'a pctx

module GenPolyKey
    (V:sig
       type 'a t
       val print : Format.formatter -> 'a t -> unit
     end)
  :
  sig
    val key : ('a,'a V.t) pkey
    val init : 'a pctx
end


(****************************************************************************)
(**                            {2 Contexts}                                 *)
(****************************************************************************)



type 'a ctx

val empty : 'a ctx

val get_unit : 'a ctx -> uctx

val set_unit : uctx -> 'a ctx -> 'a ctx

val find_unit : 'v ukey -> 'a ctx -> 'v

val find_poly : ('a,'v) pkey ->'a ctx -> 'v

val mem_unit : 'v ukey -> 'a ctx -> bool

val mem_poly : ('a,'v) pkey -> 'a ctx -> bool

val add_unit : 'v ukey -> 'v -> 'a ctx -> 'a ctx

val add_poly : ('a,'v) pkey -> 'v -> 'a ctx -> 'a ctx

val remove_unit : 'v ukey -> 'a ctx -> 'a ctx

val remove_poly : ('a,'v) pkey -> 'a ctx -> 'a ctx

val init_poly : 'a pctx -> 'a ctx -> 'a ctx

val print : Format.formatter -> 'a ctx -> unit
