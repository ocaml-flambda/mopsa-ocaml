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

open Mopsa

type alarm_kind +=
   | APyException of expr * string

let () =
  register_alarm
    {
      compare = (fun default a a' -> match a.alarm_kind, a'.alarm_kind with
                                     | APyException (e, s), APyException (e', s') -> compare_expr e e'
                                     | _ -> default a a');
      pp_token = (fun default fmt a ->
        match a.alarm_kind with
        | APyException (e, s) -> Format.fprintf fmt "PyExc(%a)" pp_expr e
        | _ -> default fmt a);
      pp_title = (fun default fmt a ->
        match a.alarm_kind with
        | APyException (e, s) -> Format.fprintf fmt "Python Exception: %s" s
        | _ -> default fmt a);
      pp_report = (fun default fmt a ->
        match a.alarm_kind with
        | APyException _ -> Format.fprintf fmt "FIXME"
        | _ -> default fmt a);
    }
