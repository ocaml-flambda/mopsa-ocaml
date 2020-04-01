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

(** Reduction between cells and smashing domains *)

open Mopsa
open Sig.Stacked.Reduction
open Universal.Ast
open Ast
open Zone

module Reduction =
struct

  let name = "c.memory.lowlevel.reductions.cell_smashing"

  let debug fmt = Debug.debug ~channel:name fmt

  let cells = Cells.Domain.id
  let smashing = Smashing.Domain.id


  let reduce exp man evals flow =
    let oe1 = man.get_eval cells evals in
    let oe2 = man.get_eval smashing evals in

    (* Reduce only when both domains did an evaluation *)
    OptionExt.apply2
      (fun e1 e2 ->
         match ekind e1, ekind e2 with
         | _, E_constant (C_top _) ->
           let evals = man.del_eval smashing evals in
           Cases.singleton evals flow

         | E_constant (C_top _),_ ->
           let evals = man.del_eval cells evals in
           Cases.singleton evals flow

         (* If both domains returned variables, refine the results *)
         | E_var _, E_var _
         | E_c_cast ({ ekind = E_var _ },_), E_var _ ->
           let cond = mk_binop e1 O_eq e2 exp.erange in
           man.post ~zone:Z_c_scalar (mk_assume cond exp.erange) flow >>= fun _ flow ->

           if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
           then
             let evals = man.del_eval cells evals |>
                         man.del_eval smashing
             in
             Cases.singleton evals flow
           else
             let evals = man.del_eval smashing evals in
             Cases.singleton evals flow


         (* Otherwise, keep the cell evaluation *)
         | _, _ ->
           let evals = man.del_eval smashing evals in
           Cases.singleton evals flow
      )
      (Cases.singleton evals flow)
      oe1 oe2

end

let () =
  register_eval_reduction (module Reduction)
