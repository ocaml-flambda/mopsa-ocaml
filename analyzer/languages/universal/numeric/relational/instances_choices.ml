(*
 * This file is part of MOPSA, a Modular Open Platform for Static Analysis.
 *
 * SPDX-FileCopyrightText: 2017-2024 The Mopsa Authors
 *
 * SPDX-License-Identifier: LGPL-3.0-or-later
 *
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License along
 * with this program. If not, see <http://www.gnu.org/licenses/>.
 *)

open Instances
open Sig.Abstraction.Simplified
open Mopsa


(* the next two open are here to force compilation of the instances before the registration of domain options, in order to be sure all domains are registered *)
open Elinainst
open Ppliteinst


let numeric_domain : (module RELATIONAL) ref = ref (module Polyhedra : RELATIONAL)

let () =
  register_domain_option "universal.numeric.relational" {
    key = "-numeric";
    category = "Numeric";
    doc = " select the relational numeric domain.";
    spec = ArgExt.Symbol (
        get_instances_names (),
        (fun name ->
           let (module M : RELATIONAL) = List.find
               (fun (module C : RELATIONAL) -> C.numeric_name = name)
               !numeric_domains in
           opt_numeric := M.numeric_name;
           numeric_domain := (module M : RELATIONAL);
           register_simplified_domain (module M)
        )
      );
    default = "polyhedra"
  }

let () =
  register_simplified_domain (module Polyhedra)
