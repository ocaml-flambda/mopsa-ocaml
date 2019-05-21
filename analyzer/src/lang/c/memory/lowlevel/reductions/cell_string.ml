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
open Sig.Stacked.Eval_reduction
open Universal.Ast
open Ast
open Zone

module Reduction =
struct

  let name = "c.memory.lowlevel.reductions.cell_string_length"

  let debug fmt = Debug.debug ~channel:name fmt

  let reduce exp man evals =
    match man.get Cells.Domain.id evals,
          man.get String_length.Domain.id evals
    with
    | Some evl1, Some evl2 ->
      let evl1', evl2' = Eval.merge (fun e1 flow1 e2 flow2 ->
          match ekind e1, ekind e2 with
          (* Constants from the cell domain should be precise, isn't it? *)
          | E_constant (C_int _), _
          | E_constant (C_c_character _), _ ->
            Some (Eval.singleton e1 flow1), None

          (* If the cell domain returned a cell variable, refine it if possible *)
          | E_var (v, _), _ ->
            let cond = mk_binop e1 O_eq e2 exp.erange in
            let flow1' = man.exec ~zone:Z_c_scalar (mk_assume cond exp.erange) flow1 in
            if Flow.get T_cur man.lattice flow1' |> man.lattice.is_bottom then
              None, None
            else
              let flow = Flow.meet man.lattice flow1' flow2 in
              begin
                match ekind e2 with
                | E_constant (C_int _) ->
                  None, Some (Eval.singleton e2 flow)

                | _ ->
                  Some (Eval.singleton e1 flow), None
              end


          (* Otherwise, keep the string evaluation, because they are
             partitioned w.r.t. to the length auxiliary variable *)
          | _, _ -> None, Some (Eval.singleton e2 flow2)

        ) evl1 evl2
      in
      man.set Cells.Domain.id evl1' evals |>
      man.set String_length.Domain.id evl2'

    | _ -> evals
end

let () =
  register_reduction (module Reduction)
