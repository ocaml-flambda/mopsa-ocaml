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

(** Signature of stacked domains 

    The signature [STACKED] is the feature-complete signature in Mopsa. 
    It represents the lowlevel unified signature used by the framework and all
    other signatures are casted to it during the construction of the top-level
    abstraction. 
    
    In addition to the access to the top-level abstraction through the manager, 
    this signature allows performing unification over a (shared) sub-abstraction
    during lattice operations ([subset], [join], [meet] and [widen]).
*)


open Core.All


(*==========================================================================*)
(**                             {1 Signature}                               *)
(*==========================================================================*)

module type STACKED =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t id
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val alarms : alarm_class list
  (** List of alarms detected by the domain *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val print: Format.formatter -> t -> unit
  (** Printer of an abstract element. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

  val subset: ('a,t) man -> ('a,'s) stack_man -> uctx -> t * 's -> t * 's -> bool * 's * 's

  val join  : ('a,t) man -> ('a,'s) stack_man -> uctx -> t * 's -> t * 's -> t * 's * 's

  val meet  : ('a,t) man -> ('a,'s) stack_man -> uctx -> t * 's -> t * 's -> t * 's * 's

  val widen : ('a,t) man -> ('a,'s) stack_man -> uctx -> t * 's -> t * 's -> t * 's * 's * bool

  val merge : t -> t * log -> t * log -> t
  (** [merge pre (post1, log1) (post2, log2)] synchronizes two divergent
      post-conditions [post1] and [post2] using a common pre-condition [pre].

      Diverging post-conditions emerge after a fork-join trajectory in the
      abstraction DAG (e.g., a reduced product).

      The logs [log1] and [log2] represent a journal of internal statements
      executed during the the computation of the post-conditions over the
      two trajectories.
  *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a,t) man -> 'a flow -> 'a flow
  (** Initialization function *)

  val exec : stmt -> ('a,t) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : expr -> ('a,t) man -> 'a flow -> 'a eval option
  (** Evaluation of expressions *)

  val ask  : ('a,'r) query -> ('a,t) man -> 'a flow -> 'r option
  (** Handler of queries *)

end


(*==========================================================================*)
(**                          {1 Registration}                               *)
(*==========================================================================*)

val register_stacked_domain : (module STACKED) -> unit
(** Register a new stacked domain *)


val find_stacked_domain : string -> (module STACKED)
(** Find a stacked domain by its name. Raise [Not_found] if no domain is found *)

val mem_stacked_domain : string -> bool
(** [mem_stacked_domain name] checks whether a stacked domain with name
    [name] is registered *)
 
val stacked_domain_names : unit -> string list
(** Return the names of registered stacked domains *) 

