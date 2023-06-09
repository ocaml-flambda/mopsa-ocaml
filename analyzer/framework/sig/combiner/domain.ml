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

(** Extended domains signatures used by combiners *)

open Core.All
open Abstraction.Domain


module type DOMAIN_COMBINER =
sig
  include DOMAIN
  val domains : DomainSet.t
  val semantics : SemanticSet.t
  val routing_table : routing_table
  val merge : t -> t * teffect -> t * teffect -> t
  val exec : DomainSet.t option -> stmt -> ('a,t) man -> 'a flow -> 'a post option
  val eval : DomainSet.t option -> expr -> ('a,t) man -> 'a flow -> 'a eval option
  val ask  : DomainSet.t option -> ('a,'r) query -> ('a,t) man -> 'a flow -> 'r option
  val print_state : DomainSet.t option -> printer -> t -> unit
  val print_expr  : DomainSet.t option -> ('a,t) man -> 'a flow -> printer -> expr -> unit
end



module DomainToCombiner(D:DOMAIN) : DOMAIN_COMBINER with type t = D.t =
struct
  include D
  let domains = DomainSet.singleton D.name
  let semantics = SemanticSet.empty
  let routing_table = empty_routing_table
  let merge pre (a1,te1) (a2,te2) =
    let e1 = get_root_effect te1 in
    let e2 = get_root_effect te2 in
    if compare_effect e1 e2 = 0 then a1 else
    if is_empty_effect e1 then a2 else
    if is_empty_effect e2 then a1
    else D.merge pre (a1,e1) (a2,e2)
  let exec targets = D.exec
  let eval targets = D.eval
  let ask targets  = D.ask
  let print_state targets = D.print_state
  let print_expr  targets = D.print_expr
end

module CombinerToDomain(T:DOMAIN_COMBINER) : DOMAIN with type t = T.t =
struct
  include T
  let merge pre (a1,e1) (a2,e2) =
    let te1 = mk_teffect e1 empty_teffect empty_teffect in
    let te2 = mk_teffect e2 empty_teffect empty_teffect in
    T.merge pre (a1,te1) (a2,te2)
  let exec stmt man flow = T.exec None stmt man flow
  let eval exp man flow  = T.eval None exp man flow
  let ask query man flow = T.ask None query man flow
  let print_state printer a = T.print_state None printer a
  let print_expr man flow printer e = T.print_expr None man flow printer e
end


module StandardToStacked(D:DOMAIN_COMBINER) : Stacked.STACKED_COMBINER with type t = D.t =
struct

  include D

  let subset man sman ctx (a,s) (a',s') =
    if a == a' then true, s, s' else
    D.subset a a', s, s'

  let join man sman ctx (a,s) (a',s') =
    if a == a' then a, s, s' else
    D.join a a', s, s'

  let meet man sman ctx (a,s) (a',s') =
    if a == a' then a, s, s' else
    D.meet a a', s, s'

  let widen man sman ctx (a,s) (a',s') =
    if a == a' then a, s, s', true else
    D.widen ctx a a', s, s', true

end
