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

(** Signature of standard domains *)


open Ast.All
open Core.All


module type DOMAIN =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  type t
  (** Type of an abstract elements. *)

  val id : t id
  (** Domain identifier *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Interface of the domain *)

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

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: uctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)

  val merge: t -> t * log -> t * log -> t
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

  val init : program -> ('a, t, 's) man -> 'a flow -> 'a flow
  (** Initialization function *)

  val exec : zone -> stmt -> ('a, t, 's) man -> 'a flow -> 'a post option
  (** Post-state of statements *)

  val eval : (zone * zone) -> expr -> ('a, t, 's) man -> 'a flow -> 'a eval option
  (** Evaluation of expressions *)

  val ask  : 'r query -> ('a, t, 's) man -> 'a flow -> 'r option
  (** Handler of queries *)

end



(** Cast a standard signature into a stacked signature *)
module MakeStacked(D:DOMAIN) : Stacked.STACKED with type t = D.t =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  type t = D.t

  let id = D.id

  let name = D.name

  let interface = D.interface

  let alarms = D.alarms

  let bottom = D.bottom

  let top = D.top

  let is_bottom = D.is_bottom

  let print = D.print


  (** {2 Lattice operators} *)
  (** ********************* *)

  let subset man ctx (a,s) (a',s') =
    if a == a' then true, s, s' else
    D.subset a a', s, s'

  let join man ctx (a,s) (a',s') =
    if a == a' then a, s, s' else
    D.join a a', s, s'

  let meet man ctx (a,s) (a',s') =
    if a == a' then a, s, s' else
    D.meet a a', s, s'

  let widen man ctx (a,s) (a',s') =
    if a == a' then a, s, s', true else
    D.widen ctx a a', s, s', true

  let merge = D.merge


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init = D.init

  let exec = D.exec

  let eval = D.eval

  let ask = D.ask


end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


(** Auto-logger lifter used when registering a domain *)
module AutoLogger(D:DOMAIN) : DOMAIN with type t = D.t =
struct
  include D

  let merge pre (a1,log1) (a2,log2) =
    if a1 == a2 then a1 else
    if Log.is_empty_log log1 then a2 else
    if Log.is_empty_log log2 then a1 else
    if (Log.compare_log log1 log2 = 0) then a1
    else D.merge pre (a1,log1) (a2,log2)


  let exec zone stmt man flow =
    D.exec zone stmt man flow |>
    OptionExt.lift @@ fun res ->
    Cases.map_log (fun log ->
        man.set_log (
          man.get_log log |> Log.add_stmt_to_log stmt
        ) log
      ) res
end

let domains : (module DOMAIN) list ref = ref []

let register_standard_domain dom =
  let module D = (val dom : DOMAIN) in
  domains := (module AutoLogger(D)) :: !domains


let find_standard_domain name =
  List.find (fun dom ->
      let module D = (val dom : DOMAIN) in
      compare D.name name = 0
    ) !domains

let mem_standard_domain name =
  List.exists (fun dom ->
      let module D = (val dom : DOMAIN) in
      compare D.name name = 0
    ) !domains

let standard_domain_names () =
  List.map (fun dom ->
      let module D = (val dom : DOMAIN) in
      D.name
    ) !domains
