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

(** String length abstraction.

    This abstract domain implements the technique presented in [1]. It
    abstracts C strings by the position of the first `\0` character in
    the base memory block. The length is kept in an underlying numeric
    domain. Therefore, the domain is implemented as a stack domain, to
    allow sharing the underlying domain with others.

    The domain is stateless. It abstraction is performed by rewriting
    statements/expressions in C into equivalent ones in Universal using
    the length variable.

    [1] M. Journault, A. Miné, A. Ouadjaout. Modular static analysis
    of string manipulations in C programs. SAS 2018. LNCS, vol. 11002.
*)

open Mopsa
open Universal.Ast
open Ast
open Universal.Zone
open Zone
open Common.Base
open Common.Points_to

module Domain =
struct

  let name = "c.memory.strings.length"

  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [Z_u_num];
    };
    ieval = {
      provides = [Z_c_low_level, Z_u_num];
      uses = [Z_c, Z_c_points_to];
    }
  }

  let init prog man flow = flow

  let exec zone stmt man flow =
    panic ~loc:__LOC__ "exec not implemented"

  let eval zone exp man flow =
    panic ~loc:__LOC__ "eval not implemented"

  let ask query man flow = None


end

let () =
  Core.Sig.Stacked.Stateless.register_stack (module Domain)
