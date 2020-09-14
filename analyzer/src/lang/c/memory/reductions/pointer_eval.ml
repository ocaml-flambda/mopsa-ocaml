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

(** Reduction rule for pointer evaluations *)

open Mopsa
open Universal.Ast
open Ast
open Sig.Reduction.Eval



module Reduction =
struct
  let name = "c.memory.reductions.pointer_eval"

  let reduce exp man rman pre results flow =
    if not @@ List.exists (fun e -> is_c_pointer_type e.etyp) results then None
    else
    match results with
      | [] -> Some (Eval.empty flow)
      | [e] -> Some (Eval.singleton e flow)
      | hd::tl ->
        (* Iterate over the list of result expressions and accumulate the most precise one *)
        let rec iter acc flow = function
          | [] -> Eval.singleton acc flow
          | hd::tl ->       
            match ekind acc, ekind hd with
            | _, E_constant (C_top _) ->
              iter acc flow tl

            | E_constant (C_top _), _ ->
              iter hd flow tl

            | E_var (v1,mode1), E_var (v2,mode2) ->
              (* Ensure that both variables are equal *)
              man.exec (mk_assume (eq acc hd exp.erange) exp.erange) flow >>% fun flow' ->
              (* Keep the strong variable as the most precise expression *)
              let precise = if var_mode v1 mode1 = STRONG then acc else hd in
              iter precise flow' tl

            | E_var _, _ | _, E_var _ ->
              (* Ensure that variable is equal the other expression *)
              man.exec (mk_assume (eq acc hd exp.erange) exp.erange) flow >>% fun flow' ->
              (* Keep the variable as the most precise expression *)
              let precise =
                match ekind hd with
                | E_var (v,_) -> hd
                | _ -> acc
              in
              iter precise flow' tl

            | _ -> iter acc flow tl
        in
        Some (iter hd flow tl)

end

let () = register_eval_reduction (module Reduction)
