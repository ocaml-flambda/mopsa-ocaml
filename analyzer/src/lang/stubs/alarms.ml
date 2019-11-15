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


type alarm_category +=
  | A_stub_invalid_requires


type alarm_detail +=
  | A_stub_invalid_requires_condition of expr

let raise_stub_invalid_requires cond range man flow =
  let cs = Flow.get_callstack flow in
  let cond' = get_orig_expr cond in
  let alarm = mk_alarm A_stub_invalid_requires (A_stub_invalid_requires_condition cond') range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow


let () =
  register_alarm_category {
      compare = (fun default a b -> default a b);
      print = (fun default fmt a ->
          match a with
          | A_stub_invalid_requires -> Format.fprintf fmt "Invalid stub requirement"
          | _ -> default fmt a
        );
    }


let () =
  register_alarm_detail {
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_stub_invalid_requires_condition e1, A_stub_invalid_requires_condition e2 ->
          compare_expr e1 e2
        | _ -> next a1 a2
      );
      print = (fun next fmt a ->
          match a with
          | A_stub_invalid_requires_condition e -> Format.fprintf fmt "invalid requirement %a" pp_expr e
          | _ -> next fmt a
        );
    };
