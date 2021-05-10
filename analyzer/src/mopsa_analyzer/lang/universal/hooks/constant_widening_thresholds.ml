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

    For better precision, a simple constant folding pass is performed on
    expressions. It replaces variables evaluated to singleton intervals with
    constants.

    For better performances, only comparisons in loops are considered.
*)

open Mopsa
open Hook
open Ast
open Numeric.Common


module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "constant_widening_thresholds"

  let debug fmt = Debug.debug ~channel:name fmt


  (** {2 Utility functions} *)
  (** ********************* *)

  (** A condition is interesting if it is a comparison between numeric
     expressions *)
  let is_interesting_condition e =
    is_numeric_type e.etyp &&
    match ekind e with
    | E_binop(op, e1, e2) ->
      is_comparison_op op &&
      is_numeric_type e1.etyp &&
      is_numeric_type e2.etyp

    | _ -> false

  (** Simple constant folding transformation. Variables that evaluate to
      singleton intervals are replaced with their value. Also, operations between
      constants are simplified. *)
  let rec constant_folding e man flow =
    match ekind e with
    | E_var (v,_) when is_int_type v.vtyp ->
      let itv = man.ask (mk_int_interval_query e) flow in
      ( match itv with
        | Bot.Nb(I.B.Finite lo, I.B.Finite hi) when Z.(lo = hi) ->
          (* Note that we return both the variable and its value. *)
          ExprSet.of_list [e; mk_z lo e.erange]
        | _ -> ExprSet.singleton e )
    | _ ->
      match expr_to_const e with
      | Some c -> ExprSet.singleton { e with ekind = E_constant c }
      | None ->
        match ekind e with
        | E_binop(O_plus, e1, e2) ->
          let s1 = constant_folding e1 man flow in
          let s2 = constant_folding e2 man flow in
          ExprSet.fold (fun e1' acc ->
              match ekind e1' with
              | E_constant (C_int n) when Z.(n = zero) ->
                ExprSet.union acc s2
              | _ ->
                ExprSet.fold (fun e2' acc ->
                    match ekind e2' with
                    | E_constant (C_int n) when Z.(n = zero) ->
                      ExprSet.add e1' acc
                    | _ ->
                      let e' = { e with ekind = E_binop(O_plus, e1', e2') } in
                      match expr_to_const e' with
                      | Some c -> ExprSet.add { e with ekind = E_constant c } acc
                      | _      -> ExprSet.add e' acc
                ) s2 acc
            ) s1 ExprSet.empty

        | E_binop(O_minus, e1, e2) ->
          let s1 = constant_folding e1 man flow in
          let s2 = constant_folding e2 man flow in
          ExprSet.fold (fun e2' acc ->
              match ekind e2' with
              | E_constant (C_int n) when Z.(n = zero) ->
                ExprSet.union acc s1
              | _ ->
                ExprSet.fold (fun e1' acc ->
                    match ekind e1' with
                    | E_constant (C_int n) when Z.(n = zero) ->
                      ExprSet.add { e with ekind = E_unop(O_minus, e2') } acc
                    | _ ->
                      let e' = { e with ekind = E_binop(O_plus, e1', e2') } in
                      match expr_to_const e' with
                      | Some c -> ExprSet.add { e with ekind = E_constant c } acc
                      | _      -> ExprSet.add e' acc
                  ) s1 acc
            ) s2 ExprSet.empty

        | E_binop(op, e1, e2) when is_comparison_op op ->
          let s1 = constant_folding e1 man flow in
          let s2 = constant_folding e2 man flow in
          ExprSet.fold (fun e1' acc ->
            ExprSet.fold (fun e2' acc ->
                  ExprSet.add { e with ekind = E_binop(op, e1', e2') } acc
                ) s2 acc
            ) s1 ExprSet.empty

        | _ -> ExprSet.singleton e

  module ThresholdSet =
    SetExt.Make(struct
      type t = var * Z.t
      let compare x1 x2 = Compare.pair compare_var Z.compare x1 x2
    end)

  let find_vars_comparisons_in_expr e =
    Visitor.fold_expr
      (fun acc ee ->
         match ekind ee with
         | E_binop(op, {ekind = E_var (v,_); etyp = T_int}, {ekind = E_constant (C_int n)})
         | E_binop(op, {ekind = E_constant (C_int n)}, {ekind = E_var (v,_); etyp = T_int})
           when is_comparison_op op ->
           Keep (ThresholdSet.add (v,n) acc)
         | _ -> VisitParts acc
      )
      (fun acc s -> VisitParts acc)
      ThresholdSet.empty e

  (** {2 Events handlers} *)
  (** ******************* *)

  let init ctx = ctx

  let loops = ref 0

  let on_before_exec route stmt man flow =
    match skind stmt with
    | S_while(cond,body) ->
      ( incr loops;
        None )
    | _ -> None

  let on_after_exec route stmt man flow post =
    match skind stmt with
    | S_while(cond,body) ->
      ( decr loops;
        None )

    | S_assume e when !loops > 0 &&
                      is_interesting_condition e ->
      (* Simplify e by replacing performing a simple constant folding pass *)
      let s = constant_folding e man flow in
      (* Search for comparisons between variables and constants in set [s] *)
      let cmps = ExprSet.fold (fun e' acc ->
          ThresholdSet.union acc (find_vars_comparisons_in_expr e')
        ) s ThresholdSet.empty in
      (* Add compared constants to the context of widening thresholds *)
      let ctx =
        ThresholdSet.fold
          (fun (v,n) acc ->
             Numeric.Common.add_widening_threshold v n acc
          ) cmps (Cases.get_ctx post) in
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
