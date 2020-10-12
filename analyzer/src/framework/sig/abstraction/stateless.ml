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

(** Stateless stacks are domains without an abstract element (e.g., iterators).
    Only transfer functions need to be defined.
*)

open Core.All


module type STATELESS =
sig

  (** {2 Domain header} *)
  (** ***************** *)

  val name : string
  (** Name of the domain *)

  val id : unit id
  (** Identifier of the domain *)

  val checks : check list
  (** List of checks performed by the domain *)


  (** {2 Transfer functions} *)
  (** ********************** *)
  val init : program -> ('a, unit) man -> 'a flow -> 'a flow
  (** Initialization routine *)

  val exec : stmt -> ('a, unit) man -> 'a flow -> 'a post option
  (** Computation of post-conditions *)

  val eval : expr -> ('a, unit) man -> 'a flow -> 'a eval option
  (** Evaluation of expressions *)

  val ask  : ('a,'r) query -> ('a, unit) man -> 'a flow -> 'r option
  (** Handler of queries *)


  (** {2 Printing} *)
  (** ************ *)

  val print_expr  : ('a,unit) man -> 'a flow -> printer -> expr -> unit
  (** Printer of an expression's value *)

end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)


(** Instrument transfer functions with some useful pre/post processing *)
module Instrument(D:STATELESS) : STATELESS =
struct
  include D

  (* Remove duplicate evaluations *)
  let eval exp man flow =
    D.eval exp man flow |>
    OptionExt.lift @@ Eval.remove_duplicates man.lattice

end


let domains : (module STATELESS) list ref = ref []

let register_stateless_domain dom =
  let module D = (val dom : STATELESS) in
  domains := (module Instrument(D)) :: !domains

let find_stateless_domain name =
  List.find (fun dom ->
      let module S = (val dom : STATELESS) in
      compare S.name name = 0
    ) !domains

let mem_stateless_domain name =
  List.exists (fun dom ->
      let module S = (val dom : STATELESS) in
      compare S.name name = 0
    ) !domains

let stateless_domain_names () =
  List.map (fun dom ->
      let module S = (val dom : STATELESS) in
      S.name
    ) !domains
