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

(** Stateless domains are domains without an abstract element (e.g., iterators).
    Only transfer functions need to be defined.
*)

open Ast.All
open Interface
open Id
open Zone
open Manager
open Flow
open Post
open Eval


module type DOMAIN =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  val name : string
  (** Name of the domain *)

  val interface : interface
  (** Zoning interface *)


  (** {2 Transfer functions} *)
  (** ********************** *)

  val init : program -> ('a, unit) man -> 'a flow -> 'a flow
  (** Initialization routine *)

  val exec : zone -> stmt -> ('a, unit) man -> 'a flow -> 'a post option
  (** Computation of post-conditions *)

  val eval : zone * zone -> expr -> ('a, unit) man -> 'a flow -> (expr, 'a) eval option
  (** Evaluation of expressions *)

  val ask  : 'r Query.query -> ('a, unit) man -> 'a flow -> 'r option
  (** Handler of queries *)

end

(** Create a full domain from a stateless domain. *)
module MakeIntermediate(D: DOMAIN) : Intermediate.DOMAIN with type t = unit =
struct

  type t = unit
  let bottom = ()
  let top = ()
  let is_bottom _ = false
  let subset _ _ = true
  let join _ _ = top
  let meet _ _ = top
  let widen _ _ _ = top
  let merge _ _ _ = top
  let print _ _ = ()

  include GenDomainId(struct
      type typ = unit
      let name = D.name
    end)

  let init = D.init

  let interface = D.interface

  let exec = D.exec
  let eval = D.eval
  let ask = D.ask
  let refine channel man flow = Channel.return flow

end

let register_domain modl =
  let module M = (val modl : DOMAIN) in
  let module D = MakeIntermediate(M) in
  Intermediate.register_domain (module D)
