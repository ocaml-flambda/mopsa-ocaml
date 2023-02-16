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

(** Soundness assumptions *)

open Mopsa
open Ast

type assumption_kind += A_py_use_type_annot of range * var * Ast.py_func_sig

let () = register_assumption {
    print = (fun next fmt -> function
              | A_py_use_type_annot (r, v, annot) ->
                 Format.fprintf fmt "Used type annotation %a(%a) -> %a@.Annotation source: %a" pp_var v Ast.pp_py_func_sig annot  (OptionExt.print pp_expr) annot.py_funcs_type_out pp_relative_range r
              | a -> next fmt a
      );
    compare = (fun next a1 a2 ->
        match a1,a2 with
        | A_py_use_type_annot (r1, v1, a1), A_py_use_type_annot (r2, v2, a2) ->
           Compare.compose
             [
               (fun () -> compare_range r1 r2);
               (fun () -> compare_var v1 v2);
               (fun () -> Ast.compare_py_func_sig a1 a2);
             ]
        | _ -> next a1 a2
      );
  }
