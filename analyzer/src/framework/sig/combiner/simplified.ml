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
open Abstraction.Simplified

module type SIMPLIFIED_COMBINER =
sig
  include SIMPLIFIED
  val domains : DomainSet.t
  val semantics : SemanticSet.t
  val routing_table : routing_table
  val exec : domain list -> stmt -> ('a,t) simplified_man -> 'a ctx -> t -> t option
  val ask  : domain list -> ('a,'r) query -> ('a,t) simplified_man -> 'a ctx -> t -> 'r option
  val print_state : domain list -> printer -> t -> unit
  val print_expr  : domain list -> ('a,t) simplified_man -> 'a ctx -> t -> printer -> expr -> unit
end



module SimplifiedToCombiner(D:SIMPLIFIED) : SIMPLIFIED_COMBINER with type t = D.t =
struct
  include D
  let domains = DomainSet.singleton D.name
  let semantics = SemanticSet.empty
  let routing_table = empty_routing_table
  let exec domains = D.exec
  let ask domains  = D.ask
  let print_state targets = D.print_state
  let print_expr  targets = D.print_expr
end

module CombinerToSimplified(T:SIMPLIFIED_COMBINER) : SIMPLIFIED with type t = T.t =
struct
  include T
  let exec stmt man ctx a = T.exec [] stmt man ctx a
  let ask query man ctx a = T.ask [] query man ctx a
  let print_state printer a = T.print_state [] printer a
  let print_expr man ctx a printer e = T.print_expr [] man ctx a printer e
end



module SimplifiedToStandard(D: SIMPLIFIED_COMBINER) : Domain.DOMAIN_COMBINER with type t = D.t =
struct

  include D

  let merge pre (post1, log1) (post2, log2) =
    let stmts1 = Log.get_log_stmts log1
    and stmts2 = Log.get_log_stmts log2 in
    D.merge pre (post1, stmts1) (post2, stmts2)


  let init prog man flow =
    let a' = D.init prog in
    set_env T_cur a' man flow

  let routing_table = empty_routing_table

  let checks = []

  let simplified_man man flow = {
    exec = (fun stmt -> man.Core.Manager.exec stmt flow |>
                        post_to_flow man |>
                        get_env T_cur man
           );
    ask = (fun query -> man.Core.Manager.ask query flow);
  }

  let exec domains =
    let f = D.exec domains in
    (fun stmt man flow ->
       let a = get_env T_cur man flow in
       if D.is_bottom a
       then
         Post.return flow |>
         OptionExt.return
       else
         f stmt (simplified_man man flow) (Flow.get_ctx flow) a |>
         OptionExt.lift @@ fun a' ->
         set_env T_cur a' man flow |>
         Post.return |>
         Cases.map_log (fun log ->
             man.set_log (
               man.get_log log |> Log.add_stmt_to_log stmt
             ) log
           )
    )

  let eval domains exp man flow = None

  let ask domains =
    let f = D.ask domains in
    (fun query man flow ->
       f query (simplified_man man flow) (Flow.get_ctx flow) (get_env T_cur man flow)
    )

  let print_expr domains =
    let f = D.print_expr domains in
    (fun man flow printer e ->
       let a = get_env T_cur man flow in
       if D.is_bottom a
       then ()
       else f (simplified_man man flow) (Flow.get_ctx flow) a printer e
    )
end





let combiners : (module SIMPLIFIED_COMBINER) list ref = ref []


let register_simplified_combiner dom =
  let module Dom = (val dom : SIMPLIFIED_COMBINER) in
  let rec iter = function
    | [] -> [dom]
    | hd :: tl ->
      let module Hd = (val hd : SIMPLIFIED_COMBINER) in
      if Hd.name = Dom.name
      then dom :: tl
      else hd :: iter tl
  in
  combiners := iter !combiners



let find_simplified_combiner name =
  List.find (fun dom ->
      let module D = (val dom : SIMPLIFIED_COMBINER) in
      compare D.name name = 0
    ) !combiners


let mem_simplified_combiner name =
  List.exists (fun dom ->
      let module D = (val dom : SIMPLIFIED_COMBINER) in
      compare D.name name = 0
    ) !combiners


let simplified_combiner_names () =
  List.map (fun dom ->
      let module D = (val dom : SIMPLIFIED_COMBINER) in
      D.name
    ) !combiners
