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
  val exec : domain list -> stmt -> ('a,t) man -> 'a flow -> 'a post option
  val eval : domain list -> expr -> ('a,t) man -> 'a flow -> 'a eval option
  val ask  : domain list -> ('a,'r) query -> ('a,t) man -> 'a flow -> 'r option
  val pretty_print : domain list -> pprinter -> expr -> ('a,t) man -> 'a flow -> unit
end



module DomainToCombiner(D:DOMAIN) : DOMAIN_COMBINER with type t = D.t =
struct
  include D
  let domains = DomainSet.singleton D.name
  let semantics = SemanticSet.empty
  let routing_table = empty_routing_table
  let exec targets = D.exec
  let eval targets = D.eval
  let ask targets  = D.ask
  let pretty_print targets = D.pretty_print
end

module CombinerToDomain(T:DOMAIN_COMBINER) : DOMAIN with type t = T.t =
struct
  include T
  let exec stmt man flow = T.exec [] stmt man flow
  let eval exp man flow  = T.eval [] exp man flow
  let ask query man flow = T.ask [] query man flow
  let pretty_print printer el man flow = T.pretty_print [] printer el man flow
end


module AutoLogger(T:DOMAIN_COMBINER) : DOMAIN_COMBINER with type t = T.t =
struct
  include T
  let merge pre (a1,log1) (a2,log2) =
    if a1 == a2 then a1 else
    if Log.is_empty_log log1 then a2 else
    if Log.is_empty_log log2 then a1 else
    if (Log.compare_log log1 log2 = 0) then a1
    else T.merge pre (a1,log1) (a2,log2)


  let exec domains =
    let f = T.exec domains in
    (fun stmt man flow ->
       f stmt man flow |>
       OptionExt.lift @@ fun res ->
       Cases.map_log (fun log ->
           man.set_log (
             man.get_log log |> Log.add_stmt_to_log stmt
           ) log
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
