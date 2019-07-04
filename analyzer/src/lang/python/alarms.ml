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


type alarm_kind += APyException
type alarm_report += RPyException of expr * string


let raise_py_alarm exn name range lattice flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm APyException ~report:(RPyException (exn,name)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true lattice flow


let () =
  register_alarm_kind {
    compare = (fun default a a' ->
        match a, a' with
        | APyException, APyException -> 0
        | _ -> default a a'
      );
    print = (fun default fmt a ->
        match a with
        | APyException -> Format.fprintf fmt "Python Exception"
        | _ -> default fmt a
      );
    };
  register_alarm_report {
    compare = (fun default a a' ->
        match a, a' with
        | RPyException (e, s), RPyException (e', s') ->
          compare_expr e e'
        | _ -> default a a'
      );
    print = (fun default fmt a ->
        match a with
        | RPyException (e, s) -> Format.fprintf fmt "Python Exception: %s" s
        | _ -> default fmt a
      );
    }
