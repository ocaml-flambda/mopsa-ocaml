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

(** Stub alarms *)

open Mopsa
open Ast



type check      += CHK_STUB_CONDITION
type alarm_kind += A_stub_invalid_requirement of expr (** condition *)
                |  A_stub_raise of string (** message *)

let () =
  register_check (fun default fmt -> function
      | CHK_STUB_CONDITION -> Format.fprintf fmt "Stub condition"
      | a -> default fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_stub_invalid_requirement _ -> CHK_STUB_CONDITION
        | A_stub_raise _ -> CHK_STUB_CONDITION
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_stub_invalid_requirement e1, A_stub_invalid_requirement e2 ->
          compare_expr e1 e2
        | A_stub_raise s1, A_stub_raise s2 ->
          String.compare s1 s2
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_stub_invalid_requirement e ->
          Format.fprintf fmt "invalid requirement '%a'" (Debug.bold pp_expr) e
        | A_stub_raise s ->
          Format.pp_print_string fmt s
        | a -> next fmt a
      );
    join = (fun next -> next);
  }

let raise_stub_invalid_requirement ?(bottom=true) cond range man flow =
  let cs = Flow.get_callstack flow in
  let cond' = get_orig_expr cond in
  let alarm = mk_alarm (A_stub_invalid_requirement cond') cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let raise_stub_alarm ?(bottom=true) msg range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_stub_raise msg) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let safe_stub_condition range man flow =
  Flow.add_safe_check CHK_STUB_CONDITION range flow
