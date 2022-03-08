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
  let name = "python.objects.dict_reduction"

  let reduce exp man rman pre results flow =
    if List.length results = 1 then
      let () = debug "reduction with %a" (format @@ Flow.print man.lattice.print) flow in
      Some (Eval.singleton (List.hd results) flow)
    else
    panic_at exp.erange "%a: results = %a" pp_expr exp (Format.pp_print_list pp_expr) results

end

let () = register_eval_reduction (module Reduction)
