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

(** Reduction rule between evaluations of cells and string length domains *)

open Mopsa
open Universal.Ast
open Sig.Stacked.Eval_reduction


module Reduction =
struct

  let name = "c.memory.lowlevel.reductions.cell_string_length"

  let debug fmt = Debug.debug ~channel:name fmt

  let reduce exp man evals =
    match man.get Cells.Domain.id evals,
          man.get String_length.Domain.id evals
    with
    | Some evl1, Some evl2 ->
      let evl1', evl2' = Eval.merge (fun e1 e2 flow ->
          match ekind e1, ekind e2 with
          (* Keep string evaluation when it is constant 0 *)
          | _, E_constant (C_int _) -> None, Some (Eval.singleton e2 flow)

          (* Otherwise, keep cells *)
          | _ -> Some (Eval.singleton e1 flow), None
        ) man.lattice evl1 evl2
      in
      man.set Cells.Domain.id evl1' evals |>
      man.set String_length.Domain.id evl2'

    | _ -> evals
end

let () =
  register_reduction (module Reduction)
