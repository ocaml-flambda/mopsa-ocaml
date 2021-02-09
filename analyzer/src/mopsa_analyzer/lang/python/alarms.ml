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


type check      += CHK_PY_UNCAUGHT_EXCEPTION
type alarm_kind += A_py_uncaught_exception of expr * string


let raise_py_uncaught_exception_alarm exn name range lattice flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_py_uncaught_exception (exn,name)) cs range in
  Flow.raise_alarm alarm ~bottom:false lattice flow


let () =
  register_check (fun default fmt a ->
        match a with
        | CHK_PY_UNCAUGHT_EXCEPTION -> Format.fprintf fmt "Uncaught Python exception"
        | _ -> default fmt a
    );
  register_alarm {
      check = (fun next -> function
                     | A_py_uncaught_exception _ -> CHK_PY_UNCAUGHT_EXCEPTION
                     | a -> next a
                   );
      compare = (fun default a a' ->
        match a, a' with
        | A_py_uncaught_exception (e, s), A_py_uncaught_exception (e', s') ->
           compare_expr e e'
        | _ -> default a a'
      );
      print = (fun default fmt a ->
        match a with
        | A_py_uncaught_exception (e, s) -> Format.fprintf fmt "Uncaught Python exception: %s" s
        | _ -> default fmt a
        );
      join = (fun next a1 a2 ->
        match a1, a2 with
        | A_py_uncaught_exception _, A_py_uncaught_exception _ ->
           if compare_alarm_kind a1 a2 = 0 then Some a1
           else None
        | _ -> next a1 a2
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


type check += CHK_PY_INVALID_TYPE_ANNOTATION
type alarm_kind += A_py_invalid_type_annotation of expr * expr

let () =
  register_check (fun next fmt -> function
      | CHK_PY_INVALID_TYPE_ANNOTATION -> Format.fprintf fmt " Type annotations"
      | a -> next fmt a)

let () =
  register_alarm {
      check = (fun next -> function
                | A_py_invalid_type_annotation _ -> CHK_PY_INVALID_TYPE_ANNOTATION
                | a -> next a);
      compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_py_invalid_type_annotation (v1, a1), A_py_invalid_type_annotation (v2, a2) ->
           Compare.compose
             [
               (fun () -> compare_expr v1 v2);
               (fun () -> compare_expr a1 a2);
             ]
        | _ -> next a1 a2);
      print = (fun next fmt -> function
                | A_py_invalid_type_annotation (v, annot) ->
                   Format.fprintf fmt "Variable '%a' does not satisfy annotation '%a'" pp_expr v pp_expr annot
                | a -> next fmt a);
      join = (fun next -> next);
    }
