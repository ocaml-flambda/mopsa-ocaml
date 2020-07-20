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
  val nodes : domain list
  val wirings : wirings
  val exec : domain list -> stmt -> ('a,t) man -> 'a flow -> 'a post option
  val eval : domain list -> expr -> ('a,t) man -> 'a flow -> 'a rewrite option
  val ask  : domain list -> ('a,'r) query -> ('a,t) man -> 'a flow -> 'r option
end


module StackedToCombiner(D:STACKED) : STACKED_COMBINER with type t = D.t =
struct
  include D
  let nodes = [D.name]
  let wirings = empty_wirings
  let exec targets = D.exec
  let eval targets = D.eval
  let ask targets  = D.ask
end

module CombinerToStacked(T:STACKED_COMBINER) : STACKED with type t = T.t =
struct
  include T
  let exec stmt man flow = T.exec [] stmt man flow
  let eval exp man flow  = T.eval [] exp man flow
  let ask query man flow = T.ask [] query man flow
end


module AutoLogger(T:STACKED_COMBINER) : STACKED_COMBINER with type t = T.t =
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


let domains : (module STACKED_COMBINER) list ref = ref []

let register_stacked_combiner dom =
  let module D = (val dom : STACKED_COMBINER) in
  domains := (module AutoLogger(D)) :: !domains

let find_stacked_combiner name =
  List.find (fun dom ->
      let module D = (val dom : STACKED_COMBINER) in
      compare D.name name = 0
    ) !domains

let mem_stacked_combiner name =
  List.exists (fun dom ->
      let module D = (val dom : STACKED_COMBINER) in
      compare D.name name = 0
    ) !domains

let stacked_combiner_names () =
  List.map (fun dom ->
      let module D = (val dom : STACKED_COMBINER) in
      D.name
    ) !domains
