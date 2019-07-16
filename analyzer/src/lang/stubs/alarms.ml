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

type alarm_kind +=
  | A_stub_invalid_require

let raise_invalid_require range ?(bottom=false) lattice flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm A_stub_invalid_require range ~cs in
  Flow.raise_alarm alarm ~bottom lattice flow


let () =
  register_alarm_kind {
      compare = (fun default a b -> default a b);
      print = (fun default fmt a ->
          match a with
          | A_stub_invalid_require -> Format.fprintf fmt "Invalid stub requirement"
          | _ -> default fmt a
        );
    };
