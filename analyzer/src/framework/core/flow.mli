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

(** Abstraction of control flows.

    Flows represent a trace partitioning the collect environments depending
    on the kind of the control flow. Not only reaching environments are
    concerned, but also environments of traces suspended at previous
    control points are kept in the flow map. In addition, flow insensitive
    contexts are also maintained in the flow abstraction.
*)

open Context
open Lattice
open Token
open Alarm
open Callstack


type 'a flow
(** A flow is a flow map augmented with an context *)

val bottom : 'a ctx -> AlarmSet.t -> 'a flow
(** Empty set of flows *)

val top : 'a ctx -> 'a flow
(** Set of all possible flows *)

val singleton: 'a ctx -> token -> 'a -> 'a flow
(** [singleton ctx tk e] returns a flow with a context [ctx]
    and a map with a single binding [tk] to environment [e] *)

val is_bottom : 'a lattice -> 'a flow -> bool
(** Emptiness test *)

val is_top : 'a lattice -> 'a flow -> bool
(** top test *)

val subset : 'a lattice -> 'a flow -> 'a flow -> bool
(** Inclusion test *)

val join : 'a lattice -> 'a flow -> 'a flow -> 'a flow
(** Abstraction union operator. *)

val join_list : 'a lattice -> empty:'a flow -> 'a flow list -> 'a flow
(** Union over a list of flows *)

val meet : 'a lattice -> 'a flow -> 'a flow -> 'a flow
(** Abstract intersection operator *)

val meet_list : 'a lattice -> empty:'a flow -> 'a flow list -> 'a flow
(** Intersection over a list of flows. *)

val widen : 'a lattice -> 'a flow -> 'a flow -> 'a flow
(** Widening operator. *)

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a flow -> unit

val get : token -> 'a lattice -> 'a flow -> 'a
(** [get tk lat flow] returns the abstract element associated to token
    [tk] in [flow]. Returns [lat.bottom] if the binding is not found. *)

val set : token -> 'a -> 'a lattice -> 'a flow -> 'a flow
(** [set tk a lat flow] overwrites the binding of token [tk] in [flow]
    with the abstract element [a]. *)

val set_bottom : token -> 'a flow -> 'a flow
(** [set_bottom tk lat flow] overwrites the binding of token [tk] in [flow]
    with a ⊥ environment. *)

val copy : token -> token -> 'a lattice -> 'a flow -> 'a flow -> 'a flow
(** [copy tk1 tk2 lat flow1 flow2] copies the environment at token
    [tk1] in [flow1] into token [tk2] in [flow2] *)

val add : token -> 'a -> 'a lattice -> 'a flow -> 'a flow
(** [add tk a lat flow] appends (by union) [a] to the existing binding
   of [tk] in [flow].  It is equivalent to [set tk (lat.join a (get tk
   lat flow)) flow] *)

val remove : token -> 'a flow -> 'a flow
(** [remove tk flow] removes token [tk] from the map of [flow] *)

val filter : (token -> 'a -> bool) -> 'a flow -> 'a flow
(** [filter f flow] keeps in [flow] all tokens [tk] verifying [f tk = true] *)

val partition : (token -> 'a -> bool) -> 'a flow -> 'a flow * 'a flow

val map : (token -> 'a -> 'a) -> 'a flow -> 'a flow

val fold : ('b -> token -> 'a -> 'b)  -> 'b -> 'a flow -> 'b

val merge :
  (token -> 'a option -> 'a option -> 'a option) ->
  (AlarmSet.t -> AlarmSet.t -> AlarmSet.t) ->
  'a lattice ->
  'a flow -> 'a flow -> 'a flow

val get_ctx : 'a flow -> 'a ctx
(** [get_all_ctx flow] retrieves the context pool from [flow] *)

val get_unit_ctx : 'a flow -> uctx

val set_unit_ctx : uctx -> 'a flow -> 'a flow

val set_ctx : 'a ctx -> 'a flow -> 'a flow
(** [set_all_ctx ctx flow] set the context pool of [flow] to
   [ctx] *)

val map_ctx : ('a ctx -> 'a ctx) -> 'a flow -> 'a flow
(** [map_all_ctx f flow] set the context of [flow] to be the
   image of the initial context of [flow] by [f] *)

val copy_ctx : 'a flow -> 'a flow -> 'a flow

val get_token_map : 'a flow -> 'a TokenMap.t

val add_alarm : ?force:bool -> alarm -> 'a lattice -> 'a flow -> 'a flow

val raise_alarm : ?force:bool -> ?bottom:bool -> alarm -> 'a lattice -> 'a flow -> 'a flow

val get_alarms : 'a flow -> AlarmSet.t

val set_alarms : AlarmSet.t -> 'a flow -> 'a flow

val remove_alarms : 'a flow -> 'a flow

val copy_alarms : 'a flow -> 'a flow -> 'a flow

val create : 'a ctx -> AlarmSet.t -> 'a TokenMap.t -> 'a flow

val get_callstack : 'a flow -> cs

val set_callstack : cs -> 'a flow -> 'a flow

val push_callstack : string -> Location.range -> 'a flow -> 'a flow

val pop_callstack : 'a flow -> Callstack.call * 'a flow

val bottom_from : 'a flow -> 'a flow
(** Empty set of flows *)
