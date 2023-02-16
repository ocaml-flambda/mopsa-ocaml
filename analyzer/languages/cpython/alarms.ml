(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2021 The MOPSA Project.                               *)
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

(** Alarms *)

open Mopsa

type check += CHK_CPYTHON_CLASS_READY
type alarm_kind += A_cpython_class_not_ready of expr

let () =
  register_check (fun next fmt -> function
      | CHK_CPYTHON_CLASS_READY -> Format.fprintf fmt "CPython class not readied"
      | a -> next fmt a)

let () =
  register_alarm {
      check = (fun next -> function
                | A_cpython_class_not_ready _ -> CHK_CPYTHON_CLASS_READY
                | a -> next a);
      compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_cpython_class_not_ready e1, A_cpython_class_not_ready e2 ->
           compare_expr e1 e2
        | _ -> next a1 a2
      );
      print = (fun next fmt -> function
                | A_cpython_class_not_ready e ->
                   Format.fprintf fmt "PyTypeObject %a has not been readied using PyType_Ready"
                     pp_expr e
                | m -> next fmt m);
      join = (fun next -> next);
    }

let raise_cpython_class_not_ready ?(bottom=true) e range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_cpython_class_not_ready e) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow
