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

(** Zones for the Python language. *)

open Mopsa
open Framework.Zone
open Universal.Ast
open Ast

type zone +=
   | Z_py
   | Z_py_obj

let () =
  register_zone {
    zone = Z_py;
    name = "Z_py";
    subset = None;
    eval = (fun exp ->
        match ekind exp with
        | E_py_undefined _
        | E_py_object _
        | E_py_list _
        | E_py_index_subscript _
        | E_py_slice_subscript _
        | E_py_attribute _
        | E_py_dict _
        | E_py_set _
        | E_py_generator_comprehension _
        | E_py_list_comprehension _
        | E_py_set_comprehension _
        | E_py_dict_comprehension _
        | E_py_call _
        | E_py_yield _
        | E_py_if _
        | E_py_tuple _
        | E_py_bytes _
        | E_py_lambda _
        | E_py_multi_compare _ -> Keep

        | _ -> Process);
  };

  register_zone {
    zone = Z_py_obj;
    name = "Z_py_object";
    subset = None;
    eval = (fun exp ->
        match ekind exp with
        | E_py_object _ -> Keep
        | _ -> Process);
  }
