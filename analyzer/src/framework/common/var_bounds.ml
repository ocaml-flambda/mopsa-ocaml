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


(** Var_bounds - Context for saving invariants of variables bounds *)

open Ast.All
open Core


let var_bounds_ctx =
  let module K = Context.GenUnitKey(struct
      type t = constant VarMap.t
      let print fmt m =
        Format.fprintf fmt "variables bounds: %a"
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt (v,b) -> Format.fprintf fmt "%a: %a" pp_var v pp_constant b)
          ) (VarMap.bindings m)
    end)
  in
  K.key


(** Add the bounds of a variable to context *)
let add_var_bounds_ctx v b uctx =
  let m = try Context.ufind var_bounds_ctx uctx with Not_found -> VarMap.empty in
  Context.uadd var_bounds_ctx (VarMap.add v b m) uctx


(** Add the bounds of a variable to flow *)
let add_var_bounds_flow v b flow =
  let ctx = add_var_bounds_ctx v b (Flow.get_unit_ctx flow) in
  Flow.set_unit_ctx ctx flow


(** Remove the bounds of a variable from context *)
let remove_var_bounds_ctx v ctx =
  try
    let m = Context.ufind var_bounds_ctx ctx in
    let mm = VarMap.remove v m in
    Context.uadd var_bounds_ctx mm ctx
  with Not_found -> ctx


(** Remove the bounds of a variable from flow *)
let remove_var_bounds_flow v flow =
  let ctx = remove_var_bounds_ctx v (Flow.get_unit_ctx flow) in
  Flow.set_unit_ctx ctx flow


(** Find the bounds of a variable in context *)
let find_var_bounds_ctx_opt v uctx =
  try
    let m = Context.ufind var_bounds_ctx uctx in
    try Some (VarMap.find v m)
    with Not_found -> None
  with Not_found -> None

