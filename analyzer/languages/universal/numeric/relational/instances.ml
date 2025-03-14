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

let opt_numeric = ref "polyhedra"

module type RELATIONAL =
sig
  include Sig.Abstraction.Simplified.SIMPLIFIED
  val numeric_name : string
  val bound_var : var -> t -> Intervals.Integer.Value.t
  val assume : stmt -> (('a, bool) Core.Query.query -> bool) -> t -> t option
  val related_vars : var -> t -> var list
  val vars : t -> var list
end

let numeric_domains : (module RELATIONAL) list ref = ref []

let register_instance (module M : RELATIONAL) =
  numeric_domains := (module M) :: !numeric_domains

let get_instances_names () =
  List.map (fun (module M : RELATIONAL) -> M.numeric_name) !numeric_domains


module Octagon = Domain.Make(struct
  type t = Oct.t
  let name = "universal.numeric.relational"
  let numeric_name = "octagon"
  let man = Oct.manager_alloc ()
end
  )

let () = register_instance (module Octagon : RELATIONAL)

module Polyhedra = Domain.Make(struct
  type t = Polka.loose Polka.t
  let name = "universal.numeric.relational"
  let numeric_name = "polyhedra"
  let man = Polka.manager_alloc_loose ()
end)

let () = register_instance (module Polyhedra : RELATIONAL)

module LinEqualities = Domain.Make(struct
    type t = Polka.equalities Polka.t
    let name = "universal.numeric.relational"
    let numeric_name = "lineq"
    let man = Polka.manager_alloc_equalities ()
  end)

let () = register_instance (module LinEqualities : RELATIONAL)

