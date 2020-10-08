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
  val exec : domain list -> stmt -> ('a,t) man -> 'a flow -> 'a post option
  val eval : domain list -> expr -> ('a,t) man -> 'a flow -> 'a eval option
  val ask  : domain list -> ('a,'r) query -> ('a,t) man -> 'a flow -> 'r option
  val print_state : domain list -> printer -> t -> unit
  val print_expr  : domain list -> ('a,t) man -> 'a flow -> printer -> expr -> unit
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
    D.merge pre (a1,e1) (a2,e2)
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
  let exec stmt man flow = T.exec [] stmt man flow
  let eval exp man flow  = T.eval [] exp man flow
  let ask query man flow = T.ask [] query man flow
  let print_state printer a = T.print_state [] printer a
  let print_expr man flow printer e = T.print_expr [] man flow printer e
end


module AutoLogger(T:DOMAIN_COMBINER) : DOMAIN_COMBINER with type t = T.t =
struct
  include T
  let merge pre (a1,te1) (a2,te2) =
    if a1 == a2 then a1 else
    if is_empty_teffect te1 then a2 else
    if is_empty_teffect te2 then a1 else
    if compare_teffect te1 te2 = 0 then a1
    else T.merge pre (a1,te1) (a2,te2)

  let exec domains =
    let f = T.exec domains in
    (fun stmt man flow ->
       f stmt man flow |>
       OptionExt.lift @@ fun res ->
       Cases.map_effects (fun effects ->
           man.set_effects (
             man.get_effects effects |>
             add_stmt_to_teffect stmt
           ) effects
         ) res
    )
end


let domains : (module DOMAIN_COMBINER) list ref = ref []

let register_standard_combiner dom =
  let module D = (val dom : DOMAIN_COMBINER) in
  domains := (module AutoLogger(D)) :: !domains

let find_standard_combiner name =
  List.find (fun dom ->
      let module D = (val dom : DOMAIN_COMBINER) in
      compare D.name name = 0
    ) !domains

let mem_standard_combiner name =
  List.exists (fun dom ->
      let module D = (val dom : DOMAIN_COMBINER) in
      compare D.name name = 0
    ) !domains

let standard_combiner_domain () =
  List.map (fun dom ->
      let module D = (val dom : DOMAIN_COMBINER) in
      D.name
    ) !domains


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
