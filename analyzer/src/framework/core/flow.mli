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
    annotations are also maintained in the flow abstraction.
*)

open Annotation
open Lattice


(****************************************************************************)
(**                             {2 Tokens}                                  *)
(****************************************************************************)

type token = ..
(** Flow tokens are used to distinguish between different control flows *)

type token += T_cur
(** Token of current (active) execution flow *)

val register_token : token Chain.info -> unit
(** Register a new token with its compare and print functions *)

val compare_token : token -> token -> int
(** Compare two tokens with a total order *)

val pp_token : Format.formatter -> token -> unit
(** Pretty printer of tokens *)


(****************************************************************************)
(**                             {2 Flows}                                   *)
(****************************************************************************)


type 'a flow
(** A flow is a flow map augmented with an annotation *)

val bottom : 'a annot -> 'a flow
(** Empty set of flows *)

val top : 'a annot -> 'a flow
(** Set of all possible flows *)

val singleton: 'a annot -> token -> 'a -> 'a flow
(** [singleton annot tk e] returns a flow with an annotation [annot]
    and a map with a single binding [tk] to environment [e] *)

val is_bottom : 'a lattice -> 'a flow -> bool
(** Emptiness test *)

val is_top : 'a lattice -> 'a flow -> bool
(** top test *)

val subset : 'a lattice -> 'a flow -> 'a flow -> bool
(** Inclusion test *)

val join : 'a lattice -> 'a flow -> 'a flow -> 'a flow
(** Abstraction union operator. *)

val join_list : 'a lattice -> ?annot:'a Annotation.annot -> 'a flow list -> 'a flow
(** Union over a list of flows *)

val meet : 'a lattice -> 'a flow -> 'a flow -> 'a flow
(** Abstract intersection operator *)

val meet_list : 'a lattice -> ?annot:'a Annotation.annot -> 'a flow list -> 'a flow
(** Intersection over a list of flows. *)

val widen : 'a lattice -> 'a flow -> 'a flow -> 'a flow
(** Widening operator. *)

val print : 'a lattice -> Format.formatter -> 'a flow -> unit
(** Pretty printer *)

val get : token -> 'a lattice -> 'a flow -> 'a
(** [get tk lat flow] returns the abstract element associated to token
    [tk] in [flow]. Returns [lat.bottom] if the binding is not found. *)

val set : token -> 'a -> 'a lattice -> 'a flow -> 'a flow
(** [set tk a lat flow] overwrites the binding of token [tk] in [flow]
    with the abstract element [a]. *)

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

val map : (token -> 'a -> 'a) -> 'a flow -> 'a flow

val fold : ('b -> token -> 'a -> 'b)  -> 'b -> 'a flow -> 'b

val merge : (token -> 'a option -> 'a option -> 'a option) -> 'a lattice -> 'a flow -> 'a flow -> 'a flow

val get_all_annot : 'a flow -> 'a annot
(** [get_all_annot flow] retrieves the annotation pool from [flow] *)

val set_all_annot : 'a annot -> 'a flow -> 'a flow
(** [set_all_annot annot flow] set the annotation pool of [flow] to
   [annot] *)

val map_all_annot : ('a annot -> 'a annot) -> 'a flow -> 'a flow
(** [map_all_annot f flow] set the annotation of [flow] to be the
   image of the initial annotation of [flow] by [f] *)

val get_annot : ('a, 'b) Annotation.key -> 'a flow -> 'b
(** [get_annot key flow] retrieves to value associated to key [key] in
   the annotations attached to flow [flow], returns [Not_found] if not
   present *)

val set_annot : ('a, 'b) Annotation.key -> 'b -> 'a flow -> 'a flow
(** [set_annot key value flow] sets the value associated to key [key]
   to be [value] in the annotations attached to flow [flow],
   overrights if already present *)

val rm_annot : ('a, 'b) Annotation.key -> 'a flow -> 'a flow
(** [rm annot key flow] removes the key binding with key [key] in the
   annotations attached to flow [flow], does not fail if [key] was not
   present *)

val mem_annot : ('a, 'b) Annotation.key -> 'a flow -> bool
(** [mem_annot key flow] checks whether an annotation is currently
   bound to [key] in [flow] *)

val copy_annot : 'a flow -> 'a flow -> 'a flow
