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
type alarm_extra += XPyException of expr * string


let raise_py_alarm exn name range lattice flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm APyException ~extra:(XPyException (exn,name)) range ~cs in
  Flow.raise_alarm alarm ~bottom:false lattice flow


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
  register_alarm_extra {
    compare = (fun default a a' ->
        match a, a' with
        | XPyException (e, s), XPyException (e', s') ->
          compare_expr e e'
        | _ -> default a a'
      );
    print = (fun default fmt a ->
        match a with
        | XPyException (e, s) -> Format.fprintf fmt "Python Exception: %s" s
        | _ -> default fmt a
      );
    }



(** Flow token for exceptions *)
type py_exc_kind =
  | Py_exc_unprecise
  | Py_exc_with_callstack of range * Callstack.cs

type token +=
  | T_py_exception of expr * string * py_exc_kind

let mk_py_unprecise_exception obj name =
  T_py_exception(obj,name,Py_exc_unprecise)

let mk_py_exception obj name ~cs range =
  T_py_exception (obj, name, Py_exc_with_callstack (range,cs))

let () =
  register_token {
    compare = (fun next tk1 tk2 ->
        match tk1, tk2 with
        | T_py_exception (e1,_,k1), T_py_exception (e2,_,k2) ->
          Compare.compose [
            (fun () -> compare_expr e1 e2);
            (fun () ->
               match k1, k2 with
               | Py_exc_unprecise, Py_exc_unprecise -> 0
               | Py_exc_with_callstack (r1, cs1), Py_exc_with_callstack (r2, cs2) ->
                 Compare.compose [
                   (fun () -> compare_range r2 r2);
                   (fun () -> Callstack.compare cs1 cs2);
                 ]
               | _ -> compare k1 k2
            );
          ]
        | _ -> next tk1 tk2
      );
    print = (fun next fmt tk ->
        match tk with
        | T_py_exception (e,_,_) -> Format.fprintf fmt "PyExc(%a)" pp_expr e
        | _ -> next fmt tk);
  }
