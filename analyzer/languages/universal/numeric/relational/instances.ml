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

open Mopsa
open Sig.Abstraction.Simplified

module Octagon = Domain.Make(struct
  type t = Oct.t
  let name = "universal.numeric.relational"
  let man = Oct.manager_alloc ()
end
)

module Polyhedra = Domain.Make(struct
  type t = Polka.loose Polka.t
  let name = "universal.numeric.relational"
  let man = Polka.manager_alloc_loose ()
end)

module LinEqualities =
  Domain.Make(struct
      type t = Polka.equalities Polka.t
      let name = "universal.numeric.relational"
      let man = Polka.manager_alloc_equalities ()
    end)

let opt_numeric = ref "polyhedra"

module type RELATIONAL =
sig
  include Sig.Abstraction.Simplified.SIMPLIFIED
  val bound_var : var -> t -> Intervals.Integer.Value.t
  val assume : stmt -> (('a, bool) Core.Query.query -> bool) -> t -> t option
  val related_vars : var -> t -> var list
  val vars : t -> var list
end

let numeric_domain : (module RELATIONAL) ref = ref (module Polyhedra : RELATIONAL)

let () =
  register_domain_option "universal.numeric.relational" {
    key = "-numeric";
    category = "Numeric";
    doc = " select the relational numeric domain.";
    spec = ArgExt.Symbol (
        ["octagon"; "polyhedra"; "lineq"],
        (function
          | "octagon"   ->
            opt_numeric := "octagon";
            numeric_domain := (module Octagon : RELATIONAL);
            register_simplified_domain (module Octagon)

          | "polyhedra" ->
            opt_numeric := "polyhedra";
            numeric_domain := (module Polyhedra : RELATIONAL);
            register_simplified_domain (module Polyhedra)

          | "lineq" ->
            opt_numeric := "lineq";
            numeric_domain := (module LinEqualities : RELATIONAL);
            register_simplified_domain (module LinEqualities)

          | _ -> assert false
        )
      );
    default = "polyhedra"
  }

let () =
  register_simplified_domain (module Polyhedra)
