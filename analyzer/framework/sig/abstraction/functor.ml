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


(** Standard signature of functor domains *)

open Core.All
open Domain
open Mopsa_utils


module type DOMAIN_FUNCTOR =
sig
  val name : string
  module Functor : functor(D:DOMAIN) -> DOMAIN
end




(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)

(** Instrument transfer functions with some useful pre/post processing *)
module Instrument(F:DOMAIN_FUNCTOR) : DOMAIN_FUNCTOR =
struct

  let name = F.name

  module Functor(D:DOMAIN) =
  struct

    module I = F.Functor(D)
    include I

    (* Add stmt to the changes of the domain *)
    let exec stmt man flow =
      if is_change_tracker_enabled () then
        I.exec stmt man flow |>
        OptionExt.lift @@ fun res ->
        Cases.map_changes (fun changes flow ->
            man.add_change stmt [] flow changes
          ) res
      else
        I.exec stmt man flow

    (* Remove duplicate evaluations *)
    let eval exp man flow =
      I.eval exp man flow |>
      OptionExt.lift @@ Eval.remove_duplicates man.lattice

  end
end


let functors : (module DOMAIN_FUNCTOR) list ref = ref []

let register_domain_functor f =
  let module F = (val f : DOMAIN_FUNCTOR) in
  let module FF = Instrument(F) in
  functors := (module FF) :: !functors

let find_domain_functor name =
  List.find (fun dom ->
      let module D = (val dom : DOMAIN_FUNCTOR) in
      compare D.name name = 0
    ) !functors

let mem_domain_functor name =
  List.exists (fun dom ->
      let module D = (val dom : DOMAIN_FUNCTOR) in
      compare D.name name = 0
    ) !functors

let domain_functor_names () =
  List.map (fun dom ->
      let module D = (val dom : DOMAIN_FUNCTOR) in
      D.name
    ) !functors
