(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2022 The MOPSA Project.                               *)
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

open Mopsa
open Ast
open Sig.Reduction.Eval

module Reduction =
  struct

    let name = "universal.toy.string_reduction"

    let reduce exp _ _ _ results flow =
      let rec aux acc flow = function
        | [] -> Eval.singleton acc flow
        | hd::tl ->
           match ekind acc, ekind hd with
           | E_var _, E_constant (C_int_interval _) -> aux acc flow tl
           | E_constant (C_int_interval _), E_var _ -> aux hd flow tl
           | _ -> aux acc flow tl
      in
      match results with
      | [] -> Some (Eval.empty flow)
      | hd::tl ->
         let r = aux hd flow tl in
         debug "reduce results=%a into %a" (Format.pp_print_list pp_expr) results Eval.print r;
         Some r

  end

let () = register_eval_reduction (module Reduction)
