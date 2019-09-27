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

(** Reduction rule between the domains of cells, string length and pointer sentinel *)

open Mopsa
open Sig.Stacked.Reduction
open Universal.Ast
open Ast
open Zone

module Reduction =
struct

  let name = "c.memory.lowlevel.reductions.cell_string_pointer"

  let debug fmt = Debug.debug ~channel:name fmt

  let reduce exp man evals flow =
    if is_c_pointer_type exp.etyp
    then Cell_pointer.Reduction.reduce exp man evals flow
    else Cell_string.Reduction.reduce exp man evals flow

end

let () =
  register_eval_reduction (module Reduction)
