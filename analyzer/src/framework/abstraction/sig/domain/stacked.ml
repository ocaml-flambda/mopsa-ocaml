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

(** Signature of stacked domains *)


open Ast.All
open Core.All


(*==========================================================================*)
(**                             {2 Signature}                               *)
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

  val dependencies : semantic list
  (** Semantic dependencies of the domain *)

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

  val eval : expr -> ('a,t) man -> 'a flow -> 'a rewrite option
  (** Evaluation of expressions *)

  val ask  : ('a,'r) query -> ('a,t) man -> 'a flow -> 'r option
  (** Handler of queries *)

end



(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


(** Auto-logger lifter used when registering a domain *)
module AutoLogger(D:STACKED) : STACKED with type t = D.t =
struct
  include D

  let merge pre (a1,log1) (a2,log2) =
    if a1 == a2 then a1 else
    if Log.is_empty_log log1 then a2 else
    if Log.is_empty_log log2 then a1 else
    if (Log.compare_log log1 log2 = 0) then a1
    else D.merge pre (a1,log1) (a2,log2)


  let exec stmt man flow =
    D.exec stmt man flow |>
    OptionExt.lift @@ fun res ->
    Cases.map_log (fun log ->
        man.set_log (
          man.get_log log |> Log.add_stmt_to_log stmt
        ) log
      ) res
end


let domains : (module STACKED) list ref = ref []

let register_stacked_domain dom =
  let module D = (val dom : STACKED) in
  domains := (module AutoLogger(D)) :: !domains

let find_stacked_domain name =
  List.find (fun dom ->
      let module D = (val dom : STACKED) in
      compare D.name name = 0
    ) !domains

let mem_stacked_domain name =
  List.exists (fun dom ->
      let module D = (val dom : STACKED) in
      compare D.name name = 0
    ) !domains

let stacked_domain_names () =
  List.map (fun dom ->
      let module D = (val dom : STACKED) in
      D.name
    ) !domains
