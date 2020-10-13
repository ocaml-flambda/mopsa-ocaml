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

(** Signature of stacked combiner domains *)

open Core.All
open Abstraction.Stacked

module type STACKED_COMBINER =
sig
  include STACKED
  val domains : DomainSet.t
  val semantics : SemanticSet.t
  val routing_table : routing_table
  val merge : t -> t * teffect -> t * teffect -> t
  val exec : domain list -> stmt -> ('a,t) man -> 'a flow -> 'a post option
  val eval : domain list -> expr -> ('a,t) man -> 'a flow -> 'a eval option
  val ask  : domain list -> ('a,'r) query -> ('a,t) man -> 'a flow -> 'r option
  val print_state : domain list -> printer -> t -> unit
  val print_expr  : domain list -> ('a,t) man -> 'a flow -> printer -> expr -> unit
end

module EmptyDomain : STACKED_COMBINER =
struct
  type t = unit
  let name = "empty"
  include GenDomainId(struct type nonrec t = t let name = name end)
  let domains = DomainSet.singleton name
  let semantics = SemanticSet.empty
  let routing_table = empty_routing_table
  let checks = []
  let bottom = ()
  let top = ()
  let is_bottom _ = false
  let print_state _ _ _ = ()
  let print_expr _ _ _ _ _ = ()
  let subset _ _ _ ((),s) ((),s') = true,s,s'
  let join _ _ _ ((),s) ((),s') = (),s,s'
  let meet _ _ _ ((),s) ((),s') = (),s,s'
  let widen _ _ _ ((),s) ((),s') = (),s,s',true
  let merge _ _ _ = ()
  let init _ _ flow = flow
  let exec _ _ _ flow = None
  let eval _ _ _ flow = None
  let ask _ _ _ _ = None
end


module StackedToCombiner(D:STACKED) : STACKED_COMBINER with type t = D.t =
struct
  include D
  let domains = DomainSet.singleton D.name
  let semantics = SemanticSet.empty
  let routing_table = empty_routing_table
  let merge pre (a1,te1) (a2,te2) =
    let e1 = get_root_effect te1 in
    let e2 = get_root_effect te2 in
    if e1 == e2 then a1 else
    if is_empty_effect e1 then a2 else
    if is_empty_effect e2 then a1
    else D.merge pre (a1,e1) (a2,e2)
  let exec targets = D.exec
  let eval targets = D.eval
  let ask targets  = D.ask
  let print_state targets = D.print_state
  let print_expr  targets = D.print_expr
end

module CombinerToStacked(T:STACKED_COMBINER) : STACKED with type t = T.t =
struct
  include T
  let merge pre (a1,e1) (a2,e2) =
    let te1 = mk_teffect e1 empty_teffect empty_teffect in
    let te2 = mk_teffect e2 empty_teffect empty_teffect in
    T.merge pre (a1,te1) (a2,te2)
  let exec stmt man flow = T.exec [] stmt man flow
  let eval exp man flow  = T.eval [] exp man flow
  let ask query man flow = T.ask [] query man flow
  let print_state printer a = T.print_state [] printer a
  let print_expr man flow printer e = T.print_expr [] man flow printer e
end
