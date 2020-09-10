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


(** Switch combiner

    A switch combination 𝒟₁;𝒟₂ creates a cartesian product in which 𝒟₁ is
    given higher priority than 𝒟₂. Transfer functions of 𝒟₁ are called first,
    and if no answer is returned 𝒟₂ is called. 
 *)

open Sig.Combiner.Stacked


(** Create a switch of two domains *)
module Make (D1:STACKED_COMBINER)(D2:STACKED_COMBINER) : STACKED_COMBINER
  with type t = D1.t * D2.t


(** Create a switch of a list of domains *)
val make : (module STACKED_COMBINER) list -> (module STACKED_COMBINER)
