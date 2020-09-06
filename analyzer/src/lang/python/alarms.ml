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


type alarm_class   += A_py_uncaught_exception
type alarm_message += A_py_uncaught_exception_msg of expr * string


let raise_py_uncaught_exception_alarm exn name range lattice flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_py_uncaught_exception_msg (exn,name)) cs range in
  Flow.raise_alarm alarm ~bottom:false lattice flow


let () =
  register_alarm_class (fun default fmt a ->
        match a with
        | A_py_uncaught_exception -> Format.fprintf fmt "Uncaught Python exception"
        | _ -> default fmt a
    );
  register_alarm_message {
      classifier = (fun next -> function
                     | A_py_uncaught_exception_msg _ -> A_py_uncaught_exception
                     | a -> next a
                   );
      compare = (fun default a a' ->
        match a, a' with
        | A_py_uncaught_exception_msg (e, s), A_py_uncaught_exception_msg (e', s') ->
           compare_expr e e'
        | _ -> default a a'
      );
      print = (fun default fmt a ->
        match a with
        | A_py_uncaught_exception_msg (e, s) -> Format.fprintf fmt "Uncaught Python exception: %s@." s
        | _ -> default fmt a
      );
    }

(** Flow token for exceptions *)
type py_exc_kind =
  | Py_exc_unprecise
  | Py_exc_with_callstack of range * callstack

type token +=
  | T_py_exception of expr * string * py_exc_kind

let mk_py_unprecise_exception obj name =
  T_py_exception(obj,name,Py_exc_unprecise)

let mk_py_exception obj name ~cs range =
  T_py_exception (obj, name, Py_exc_with_callstack (range,cs))

let pp_py_exc_kind fmt = function
  | Py_exc_unprecise -> ()
  | Py_exc_with_callstack (range,cs) -> Format.fprintf fmt "%a@,%a" pp_range range pp_callstack_short cs

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
                   (fun () -> compare_range r1 r2);
                   (fun () -> compare_callstack cs1 cs2);
                 ]
               | _ -> compare k1 k2
            );
          ]
        | _ -> next tk1 tk2
      );
    print = (fun next fmt tk ->
        match tk with
        | T_py_exception (_,str,k) -> Format.fprintf fmt "@[<hv 2>PyExc(%s)@,%a@]" str pp_py_exc_kind k
        | _ -> next fmt tk);
  }
