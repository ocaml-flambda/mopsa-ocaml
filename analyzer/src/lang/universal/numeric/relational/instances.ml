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


let opt_numeric = ref "polyhedra"

let () =
  register_domain_option "universal.numeric.relational" {
    key = "-numeric";
    category = "Numeric";
    doc = " select the relational numeric domain.";
    spec = ArgExt.Symbol (
        ["octagon"; "polyhedra"],
        (function
          | "octagon"   ->
            opt_numeric := "octagon";
            Framework.Core.Sig.Domain.Simplified.register_domain (module Octagon)

          | "polyhedra" ->
            opt_numeric := "polyhedra";
            Framework.Core.Sig.Domain.Simplified.register_domain (module Polyhedra)

          | _ -> assert false
        )
      );
    default = "polyhedra"
  }

let () =
  Framework.Core.Sig.Domain.Simplified.register_domain (module Polyhedra)
