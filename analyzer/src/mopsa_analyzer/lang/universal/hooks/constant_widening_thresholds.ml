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

(** Hook to collect widening thresholds.

    The heuristic is simple: the hook tracks all comparisons between a
    numeric variable and a constant. It then adds the constant to the
    context of thresholds associated to the variable.
*)

open Mopsa
open Hook
open Ast


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "constant_widening_thresholds"

  let debug fmt = Debug.debug ~channel:name fmt


  (** {2 Utility functions} *)
  (** ********************* *)

  let is_var_comparison_expr e =
    is_numeric_type e.etyp &&
    Visitor.exists_expr
      (fun ee ->
         match ekind ee with
         | E_binop(op, {ekind = E_var _; etyp = T_int}, {ekind = E_constant (C_int _)})
         | E_binop(op, {ekind = E_constant (C_int _)}, {ekind = E_var _; etyp = T_int})
           when is_comparison_op op ->
           true
         | _ -> false
      )
      (fun s -> false)
      e

  let find_vars_comparisons_in_expr e =
    Visitor.fold_expr
      (fun acc ee ->
         match ekind ee with
         | E_binop(op, {ekind = E_var (v,_); etyp = T_int}, {ekind = E_constant (C_int n)})
         | E_binop(op, {ekind = E_constant (C_int n)}, {ekind = E_var (v,_); etyp = T_int})
           when is_comparison_op op ->
           Keep ((v,n) :: acc)
         | _ -> VisitParts acc
      )
      (fun acc s -> VisitParts acc)
      [] e

  (** {2 Events handlers} *)
  (** ******************* *)

  let init ctx = ctx

  let on_before_exec route stmt man flow = None

  let on_after_exec route stmt man flow post =
    match skind stmt with
    | S_assume e when is_var_comparison_expr e ->
      let cmps = find_vars_comparisons_in_expr e in
      let ctx =
        List.fold_left
          (fun acc (v,n) ->
             Numeric.Common.add_widening_threshold v n acc
          ) (Cases.get_ctx post) cmps in
      Some ctx

    | S_remove {ekind = E_var(v,_); etyp = T_int} ->
      let ctx = Cases.get_ctx post |>
                Numeric.Common.remove_widening_thresholds v in
      Some ctx

    | _ -> None

  let on_before_eval route semantic exp man flow = None

  let on_after_eval route semantic exp man flow eval = None

  let on_finish man flow = ()


end

let () =
  register_hook (module Hook)
