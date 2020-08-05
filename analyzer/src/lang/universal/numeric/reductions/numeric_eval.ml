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

(** Reduction rule for numeric evaluations *)

open Mopsa
open Ast
open Sig.Reduction.Eval



module Reduction =
struct
  let name = "universal.numeric.reductions.numeric_eval"

  let reduce exp man rman pre results flow =
    if List.exists (fun e -> is_numeric_type e.etyp) results then None
    else
      let () = Debug.debug ~channel:name "reduce(%a): %a" pp_expr exp Format.(pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ∧ ") pp_expr) results in
    match results with
      | [] -> Some (Eval.empty_singleton flow)
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

            | E_constant (C_int a), E_constant (C_int b) ->
              if Z.(a = b) then iter acc flow tl else Eval.empty_singleton (Flow.bottom_from flow)

            | E_constant (C_bool a), E_constant (C_bool b) ->
              if a = b then iter acc flow tl else Eval.empty_singleton (Flow.bottom_from flow)

            | E_constant (C_int_interval(a,b)), E_constant(C_int c) ->
              if Z.(a <= c && c <= b) then iter hd flow tl else Eval.empty_singleton (Flow.bottom_from flow)

            | E_constant(C_int c), E_constant (C_int_interval(a,b)) ->
              if Z.(a <= c && c <= b) then iter acc flow tl else Eval.empty_singleton (Flow.bottom_from flow)

            | E_constant (C_int_interval(a,b)), E_constant(C_bool c) ->
              let d = if c then Z.one else Z.zero in
              if Z.(a <= d && d <= b) then iter hd flow tl else Eval.empty_singleton (Flow.bottom_from flow)

            | E_constant(C_bool c), E_constant (C_int_interval(a,b)) ->
              let d = if c then Z.one else Z.zero in
              if Z.(a <= d && d <= b) then iter acc flow tl else Eval.empty_singleton (Flow.bottom_from flow)

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
