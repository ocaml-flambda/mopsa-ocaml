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

(** Packing strategy *)

open Mopsa
open Context


(** Signature of a packing strategy *)
module type STRATEGY = functor(Domain:Sig.Domain.Simplified.DOMAIN) ->
sig

  val name : string
  (** Name of the packing strategy *)

  type key
  (** Keys to identify packs *)

  val compare : key -> key -> key
  (** Total order of packing keys *)

  val print : Format.formatter -> key -> unit
  (** Pretty printer of packing keys *)

  module Map : MapExtSig.S with type key = key
  (** Maps from keys to abstract elements *)

  val subset : Domain.t Map.t -> Domain.t Map.t -> bool
  (** Inclusion test *)

  val join : Domain.t Map.t -> Domain.t Map.t -> Domain.t Map.t
  (** Abstract union *)

  val meet : Domain.t Map.t -> Domain.t Map.t -> Domain.t Map.t
  (** Abstract intersection *)

  val widen : uctx -> Domain.t Map.t -> Domain.t Map.t -> Domain.t Map.t
  (** Widening operator *)

  val merge : Domain.t Map.t -> Domain.t Map.t * block -> Domain.t Map.t * block -> Domain.t Map.t
  (** Merge of divergent states *)

  val init : program -> Domain.t Map.t
  (** Initial state *)

  val exec : stmt -> Domain.t Map.t -> Domain.t Map.t option
  (** Abstract transformer of statements *)

end
