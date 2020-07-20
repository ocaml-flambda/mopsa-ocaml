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


(** Stacked signature of functor domains *)

open Core.All
open Stacked

module type STACKED_COMBINER_FUNCTOR =
sig
  val name : string
  module Functor : functor(D:STACKED_COMBINER) -> STACKED_COMBINER
end




(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)

(** Module to automatically log statements of a functor *)
module AutoLogger(F:STACKED_COMBINER_FUNCTOR) : STACKED_COMBINER_FUNCTOR =
struct
  include F
  module Functor(D:STACKED_COMBINER) =
  struct
    include D
    let exec domains =
      let f = D.exec domains in
      (fun stmt man flow ->
         f stmt man flow |> OptionExt.lift @@ fun res ->
         Cases.map_log (fun log ->
             man.set_log (
               man.get_log log |> Log.add_stmt_to_log stmt
             ) log
           ) res
      )
  end
end


let functors : (module STACKED_COMBINER_FUNCTOR) list ref = ref []

let register_stacked_combiner_functor f =
  let module F = (val f : STACKED_COMBINER_FUNCTOR) in
  let module FF = AutoLogger(F) in
  functors := (module FF) :: !functors

let find_stacked_combiner_functor name =
  List.find (fun dom ->
      let module D = (val dom : STACKED_COMBINER_FUNCTOR) in
      compare D.name name = 0
    ) !functors

let mem_stacked_combiner_functor name =
  List.exists (fun dom ->
      let module D = (val dom : STACKED_COMBINER_FUNCTOR) in
      compare D.name name = 0
    ) !functors

let stacked_combiner_functor_names () =
  List.map (fun dom ->
      let module D = (val dom : STACKED_COMBINER_FUNCTOR) in
      D.name
    ) !functors
