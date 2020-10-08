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

  include F

  module Functor(D:DOMAIN) =
  struct

    include D

    let merge pre (a1,e1) (a2,e2) =
      if a1 == a2 then a1 else
      if is_empty_effect e1 then a2 else
      if is_empty_effect e2 then a1 else
      if compare_effect e1 e2 = 0 then a1
      else D.merge pre (a1,e1) (a2,e2)


    (* Add stmt to the effects of the domain *)
    let exec stmt man flow =
      D.exec stmt man flow |>
      OptionExt.lift @@ fun res ->
      Cases.map_effects (fun effects ->
          man.set_effects (
            man.get_effects effects |>
            add_stmt_to_teffect stmt
          ) effects
        ) res

    (* Remove duplicate evaluations *)
    let eval exp man flow =
      D.eval exp man flow |>
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
