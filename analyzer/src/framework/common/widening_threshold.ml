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


(** Context for stocking widening thresholds *)

open Ast.All
open Core


(** Widening thresholds are represented as a set of expressions *)
module Thresholds = SetExt.Make(struct type t = expr let compare = compare_expr end)


(** Context key for indexing widening thresholds within the context table *)
let widening_thresholds_ctx_key =
  let module K = Context.GenUnitKey(
    struct
      type t = Thresholds.t
      let print fmt t =
        Format.fprintf fmt "widening thresholds: %a"
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
          (Thresholds.elements t)
    end
  )
  in
  K.key


(** Add a condition expression to widening thresholds *)
let add_widening_threshold cond flow =
  let ctx = Flow.get_ctx flow in
  let thresholds = try Context.find_unit widening_thresholds_ctx_key ctx with Not_found -> Thresholds.empty in
  let thresholds' = Thresholds.add cond thresholds in
  let ctx' = Context.add_unit widening_thresholds_ctx_key thresholds' ctx in
  Flow.set_ctx ctx' flow


(** Return the list of widening thresholds *)
let get_widening_thresholds flow =
  let ctx = Flow.get_ctx flow in
  try Context.find_unit widening_thresholds_ctx_key ctx |>
      Thresholds.elements
  with Not_found -> []


(** Fold over widening thresholds *)
let fold_widening_thresholds f x0 flow =
  let ctx = Flow.get_ctx flow in
  try
    let thresholds = Context.find_unit widening_thresholds_ctx_key ctx in
    Thresholds.fold f thresholds x0
  with Not_found ->
    x0
