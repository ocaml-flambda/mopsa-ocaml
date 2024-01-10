(****************************************************************************)
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
  val exec : DomainSet.t option -> stmt -> ('a,unit) man -> 'a flow -> 'a post option
  val eval : DomainSet.t option -> expr -> ('a,unit) man -> 'a flow -> 'a eval option
  val ask  : DomainSet.t option -> ('a,'r) query -> ('a,unit) man -> 'a flow -> ('a, 'r) cases option
  val print_expr : DomainSet.t option -> ('a,unit) man -> 'a flow -> printer -> expr -> unit
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
  let print_expr targets = D.print_expr
end

module CombinerToStateless(T:STATELESS_COMBINER) : STATELESS =
struct
  include T
  let exec stmt man flow = T.exec None stmt man flow
  let eval exp man flow  = T.eval None exp man flow
  let ask query man flow = T.ask None query man flow
  let print_expr man flow printer e = T.print_expr None man flow printer e
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

  let subset _ _ _ _ = true
  let join _ _ _ _ = ()
  let meet _ _ _ _ = ()
  let widen _ _ _ _ = ()

  let print_state _ _ () = ()

end
