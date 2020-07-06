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

(** Semantic dependencies

    Domains do not have explicit dependencies on particular abstractions.
    Instead, domains can depend on an external concrete semantics implemented
    by some abstraction determined at runtime.

    For example, consider the following case:

                                     |
                                     v
                                  +------+
                                  |  D1  |
                                  +------+
                                   |    |
        semantic dependency #1 +---+    +---+ semantic dependency #2
                               |            |
                               v            v
                            +------+     +------+
                            |  D2  |     |  D3  |
                            +------+     +------+

    -  Domain D1 receives commands (execute a statement, evaluate an expression)
    expressed in its concrete semantics,

    -  For modularity reasons, D1 maintains part of its abstraction locally
    and expresses other parts as a function of two other abstractions.

    - D1 communicate with its dependencies through commands expressed in their
    concrete semantics, and does not rely on internal states of D2 and D3.
*)


type semantic
(** semantic dependency *)

val mk_semantic: name:string -> domain:string -> semantic
(** Create a semantic from a name and a domain *)

val any_semantic : semantic
(** Wildcard dependency *)

val domain_of_semantic : semantic -> string
(** Get the domain of a semantic dependency *)

val name_of_semantic : semantic -> string
(** Get the name of a semantic dependency *)

val compare_semantic : semantic -> semantic -> int
(** Compare two semantic dependencies *)

val pp_semantic : Format.formatter -> semantic -> unit
(** Print a semantic dependency *)
