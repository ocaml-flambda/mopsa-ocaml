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
open Abstraction.Stateless


module type STATELESS_COMBINER =
sig
  include STATELESS
  val domains : DomainSet.t
  val semantics : SemanticSet.t
  val routing_table : routing_table
  val exec : domain list -> stmt -> ('a,unit) man -> 'a flow -> 'a post option
  val eval : domain list -> expr -> ('a,unit) man -> 'a flow -> 'a eval option
  val ask  : domain list -> ('a,'r) query -> ('a,unit) man -> 'a flow -> 'r option
  val pretty_print : domain list -> Pretty_printer.printer -> expr -> ('a,unit) man -> 'a flow -> unit
end



module StatelessToCombiner(D:STATELESS) : STATELESS_COMBINER =
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

module CombinerToStateless(T:STATELESS_COMBINER) : STATELESS =
struct
  include T
  let exec stmt man flow = T.exec [] stmt man flow
  let eval exp man flow  = T.eval [] exp man flow
  let ask query man flow = T.ask [] query man flow
  let pretty_print printer el man flow = T.pretty_print [] printer el man flow
end





module StatelessToDomain(S:STATELESS_COMBINER) : Domain.DOMAIN_COMBINER with type t = unit =
struct

  include S

  type t = unit
  let bottom = ()
  let top = ()
  let is_bottom () = false
  let merge _ _ _ = ()
  let print _ _ = ()

  let subset () () = true
  let join () () = ()
  let meet () () = ()
  let widen _ () () = ()

end



(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


let combiners : (module STATELESS_COMBINER) list ref = ref []

let register_stateless_combiner dom =
  combiners := dom :: !combiners

let find_stateless_combiner name =
  List.find (fun dom ->
      let module S = (val dom : STATELESS_COMBINER) in
      compare S.name name = 0
    ) !combiners

let mem_stateless_combiner name =
  List.exists (fun dom ->
      let module S = (val dom : STATELESS_COMBINER) in
      compare S.name name = 0
    ) !combiners

let stateless_combiner_names () =
  List.map (fun dom ->
      let module S = (val dom : STATELESS_COMBINER) in
      S.name
    ) !combiners
