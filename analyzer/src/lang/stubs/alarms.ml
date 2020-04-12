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
open Sig.Stacked.Manager


type alarm_class   += A_stub_invalid_requires
type alarm_message += A_stub_invalid_requires_condition of expr

let raise_stub_invalid_requires cond range man flow =
  let cs = Flow.get_callstack flow in
  let cond' = get_orig_expr cond in
  let alarm = mk_alarm (A_stub_invalid_requires_condition cond') cs range in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow


let () =
  register_alarm_class (fun default fmt -> function
      | A_stub_invalid_requires -> Format.fprintf fmt "Invalid stub requirement"
      | a -> default fmt a
    )


let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_stub_invalid_requires_condition _ -> A_stub_invalid_requires
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_stub_invalid_requires_condition e1, A_stub_invalid_requires_condition e2 ->
          compare_expr e1 e2
        | _ -> next a1 a2
      );
      print = (fun next fmt -> function
        | A_stub_invalid_requires_condition e -> Format.fprintf fmt "invalid requirement '%a'" (Debug.bold pp_expr) e
        | a -> next fmt a
      );
  }





type alarm_class   += A_stub_alarm
type alarm_message += A_stub_alarm_body of string

let raise_stub_alarm msg range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_stub_alarm_body msg) cs range in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow


let () =
  register_alarm_class (fun default fmt -> function
      | A_stub_alarm -> Format.fprintf fmt "Stub alarm"
      | a -> default fmt a
    )


let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_stub_alarm_body _ -> A_stub_alarm
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_stub_alarm_body m1, A_stub_alarm_body m2 -> compare m1 m2
        | _ -> next a1 a2
      );
      print = (fun next fmt -> function
        | A_stub_alarm_body m -> Format.pp_print_string fmt m
        | a -> next fmt a
      );
  };
