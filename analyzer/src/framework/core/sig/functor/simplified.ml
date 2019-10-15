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

(** Simplified interface of functor domains. *)

open Ast.All
open Zone
open Query
open Lattice
open Context
open Channel
open Id


(** {2 Signature of functor domains} *)
(** ******************************** *)

module type FUNCTOR =
  functor(Arg:Domain.Simplified.DOMAIN) -> Domain.Simplified.DOMAIN



(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let functors : (string * (module FUNCTOR)) list ref = ref []


let register_functor name dom =
  let module Dom = (val dom : FUNCTOR) in
  let rec iter = function
    | [] -> [name,dom]
    | (hdname,hd) :: tl ->
      let module Hd = (val hd : FUNCTOR) in
      if hdname = name
      then (name,dom) :: tl
      else (hdname,hd) :: iter tl
  in
  functors := iter !functors



let find_functor name =
  List.find (fun (name',dom) ->
      let module D = (val dom : FUNCTOR) in
      compare name name' = 0
    ) !functors |>
  snd


let mem_functor name =
  List.exists (fun (name',dom) ->
      let module D = (val dom : FUNCTOR) in
      compare name name' = 0
    ) !functors


let names () =
  List.map (fun (name,dom) -> name) !functors
