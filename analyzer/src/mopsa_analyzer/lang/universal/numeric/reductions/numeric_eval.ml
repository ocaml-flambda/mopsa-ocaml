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
    (* No reduction if no numeric expression found *)
    if not @@ List.exists
        (fun e ->
           let n = get_expr_translation "Universal" e in
           is_numeric_type n.etyp
        ) results
    then None
    else
    (* Simplify constant expressions *)
    let results' =
      results
      |> List.map (fun e ->
          let n = get_expr_translation "Universal" e in
          match expr_to_const n with
          | Some c -> add_expr_translation "Universal" { n with ekind = E_constant c } e
          | None -> e
        )
      (* Convert boolean values to numeric values to simplify reduction *)
      |> List.map (fun e ->
          let n = get_expr_translation "Universal" e in
          match ekind n with
          | E_constant (C_bool true) -> add_expr_translation "Universal" { n with ekind = E_constant (C_int Z.one); etyp = T_int } e
          | E_constant (C_bool false) -> add_expr_translation "Universal" { n with ekind = E_constant (C_int Z.zero); etyp = T_int } e
          | _ -> e
        )
    in
    match results' with
      | [] -> Some (Eval.empty flow)
      | [e] -> Some (Eval.singleton e flow)
      | hd::tl ->
        (* Iterate over the list of result expressions and accumulate the most precise one *)
        let rec iter acc flow = function
          | [] -> Eval.singleton acc flow
          | hd::tl ->
            let nacc = get_expr_translation "Universal" acc in
            let nhd = get_expr_translation "Universal" hd in
            match ekind nacc, ekind nhd with
            (* Top rules *)
            | _, E_constant (C_top _) ->
              iter acc flow tl

            | E_constant (C_top _), _ ->
              iter hd flow tl

            (* Integer expressions *)
            | E_constant (C_int a), E_constant (C_int b) ->
              if Z.(a = b) then iter acc flow tl else Eval.empty flow

            | E_constant (C_int_interval(a,b)), E_constant(C_int c) ->
              if Z.(a <= c && c <= b) then iter hd flow tl else Eval.empty flow

            | E_constant(C_int c), E_constant (C_int_interval(a,b)) ->
              if Z.(a <= c && c <= b) then iter acc flow tl else Eval.empty flow

            | E_constant(C_int_interval (a,b)), E_constant (C_int_interval(c,d)) ->
              let lo = Z.max a c and hi = Z.min b d in
              if Z.(lo <= hi) then
                let acc = add_expr_translation "Universal" (mk_z_interval lo hi exp.erange) acc in
                iter acc flow tl
              else
                Eval.empty flow

            (* Variables *)
            | E_var (v1,mode1), E_var (v2,mode2) ->
              (* Ensure that both variables are equal *)
              man.exec (mk_assume (eq acc hd exp.erange) exp.erange) flow >>% fun flow' ->
              (* Keep the strong variable as the most precise expression *)
              let precise = if var_mode v1 mode1 = STRONG then acc else hd in
              iter precise flow' tl

            | E_var _, _
            | _, E_var _ ->
              (* Ensure that variable is equal the other expression *)
              man.exec (mk_assume (eq acc hd exp.erange) exp.erange) flow >>% fun flow' ->
              (* Keep the variable as the most precise expression *)
              let precise =
                match ekind nhd with
                | E_var (v,_) -> hd
                | _ -> acc
              in
              iter precise flow' tl

            (* Logic expressions *)
            | E_binop(op1,_,_), E_binop(op2,_,_) when (is_comparison_op op1 || is_logic_op op1)
                                                   && (is_comparison_op op2 || is_logic_op op2) ->
              (* Transform as a conjunction *)
              let acc' = add_expr_translation "Universal" (mk_log_and acc hd exp.erange) acc in
              iter acc' flow tl

            (* constant AND compare : keep compare *)
            | E_constant (C_int n), E_binop(op,_,_)
            | E_binop(op,_,_), E_constant (C_int n) when (is_comparison_op op || is_logic_op op) ->
              let precise =
                match ekind nhd with E_binop _ -> hd | _ -> acc
              in
              iter precise flow tl

            (* Keep other constants *)
            | E_constant _, _ ->
              iter hd flow tl

            | _, E_constant _ ->
              iter acc flow tl


            | _ -> iter acc flow tl
        in
        Some (iter hd flow tl)

end

let () = register_eval_reduction (module Reduction)
