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


(** Get the callstack of the function that called the stub. Used to attach
    alarms at proper locations outside the body of the stub. *)
let patch_callstack cs range =
  let rec aux = function
    | [] -> []
    | hd::tl as x ->
      if compare_range hd.call_range range = 0
      then aux tl
      else x
  in
  aux cs


type check      += CHK_STUB_INVALID_REQUIRES
type alarm_kind += A_stub_invalid_requires of expr (** condition *)

let raise_stub_invalid_requires ?(bottom=true) cond range man flow =
  let cs = Flow.get_callstack flow in
  let cond' = get_orig_expr cond in
  let alarm = mk_alarm (A_stub_invalid_requires cond') (patch_callstack cs range) range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


let () =
  register_check (fun default fmt -> function
      | CHK_STUB_INVALID_REQUIRES -> Format.fprintf fmt "Invalid stub requirement"
      | a -> default fmt a
    )


let () =
  register_alarm {
    check = (fun next -> function
        | A_stub_invalid_requires _ -> CHK_STUB_INVALID_REQUIRES
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_stub_invalid_requires e1, A_stub_invalid_requires e2 ->
          compare_expr e1 e2
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_stub_invalid_requires e ->
          Format.fprintf fmt "invalid requirement '%a'" (Debug.bold pp_expr) e
        | a -> next fmt a
      );
    join = (fun next -> next);
  }


type check      += CHK_STUB_ALARM
type alarm_kind += A_stub_alarm of string

let raise_stub_alarm ?(bottom=true) msg range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_stub_alarm msg) (patch_callstack cs range) range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


let () =
  register_check (fun default fmt -> function
      | CHK_STUB_ALARM -> Format.fprintf fmt "Stub alarm"
      | a              -> default fmt a
    )


let () =
  register_alarm {
    check = (fun next -> function
        | A_stub_alarm _ -> CHK_STUB_ALARM
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_stub_alarm m1, A_stub_alarm m2 -> String.compare m1 m2
        | _ -> next a1 a2
      );
      print = (fun next fmt -> function
        | A_stub_alarm m -> Format.pp_print_string fmt m
        | a -> next fmt a
      );
    join = (fun next -> next);
  };
