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
open Sig.Stacked.Reduction
open Universal.Ast
open Ast
open Zone

module Reduction =
struct

  let name = "c.memory.lowlevel.reductions.cell_smashing"

  let debug fmt = Debug.debug ~channel:name fmt

  let reduce exp man evals =
    match man.get_eval Cells.Domain.id evals,
          man.get_eval Smashing.Domain.id evals
    with
    | Some evl1, Some evl2 ->
      let evl1', evl2' = Eval.merge (fun e1 flow1 e2 flow2 ->
          match ekind e1, ekind e2 with
          (* Constants from the cell domain should be precise, isn't it? *)
          | E_constant (C_int _), _
          | E_constant (C_c_character _), _ ->
            Some (Eval.singleton e1 flow1), None

          (* Ensure that the cell and the smash are equal *)
          | E_var _, E_var ({ vkind = Smashing.Domain.V_c_smash smash }, _)  ->
            let cond = mk_binop e1 O_eq e2 exp.erange in
            let flow1' = Smashing.Domain.add_smash smash exp.erange (man.get_man Smashing.Domain.id) flow1 |>
                         Post.to_flow man.lattice |>
                         man.exec ~zone:Z_c_scalar (mk_assume cond exp.erange)
            in
            if Flow.get T_cur man.lattice flow1' |> man.lattice.is_bottom then
              None, None
            else
              let flow = Flow.meet man.lattice flow1' flow2 in
              Some (Eval.singleton e1 flow), None

          (* Cell is precise if smash does not return a variable *)
          | E_var _, _  ->
            Some (Eval.singleton e1 flow1), None

          (* Smash is precise if cell does not return a variable *)
          | _, E_var _  ->
            Some (Eval.singleton e1 flow1), None


          | _ ->
            None, Some (Eval.singleton e2 flow2)


        ) evl1 evl2
      in
      man.set_eval Cells.Domain.id evl1' evals |>
      man.set_eval Smashing.Domain.id evl2'

    | _ -> evals
end

let () =
  register_eval_reduction (module Reduction)
