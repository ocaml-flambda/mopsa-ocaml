(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2021 The MOPSA Project.                               *)
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

(** Trace markers *)

open Ast.Stmt
open Ast.Visitor
open Mopsa_utils

type marker = ..

type marker_info = {
  marker_name : (marker -> string) -> marker -> string;
  marker_print : (Format.formatter -> marker -> unit) -> Format.formatter -> marker -> unit;
  marker_compare : (marker -> marker -> int) -> marker -> marker -> int;
}

let compare_marker_chain =
  ref (fun m1 m2 -> Stdlib.compare m1 m2)

let pp_marker_chain  =
  ref (fun fmt mk -> Exceptions.panic "marker not registered")

let name_marker_chain =
  ref (fun m -> Exceptions.panic "marker not registered")

let compare_marker m1 m2 = !compare_marker_chain m1 m2
let pp_marker fmt m = !pp_marker_chain fmt m
let get_marker_name m = !name_marker_chain m

let register_marker info =
  pp_marker_chain := info.marker_print !pp_marker_chain;
  compare_marker_chain := info.marker_compare !compare_marker_chain;
  name_marker_chain := info.marker_name !name_marker_chain

type stmt_kind +=
  | S_add_marker of marker

let mk_add_marker m range =
  mk_stmt (S_add_marker m) range

let opt_enabled_markers = ref []

let enable_marker name =
  opt_enabled_markers := name :: !opt_enabled_markers

let disable_marker name =
  opt_enabled_markers := List.filter (fun name' -> name' <> name) !opt_enabled_markers

let is_marker_enabled m =
  match !opt_enabled_markers with
  | [] -> true
  | l ->
    let name = get_marker_name m in
    List.exists (fun name' -> String.equal name name') !opt_enabled_markers

let () = register_stmt_with_visitor {
    print = (fun next fmt stmt ->
        match skind stmt with
        | S_add_marker m ->
          Format.fprintf fmt "add-marker(%a)" pp_marker m
        | _ ->
          next fmt stmt
      );
    compare = (fun next s1 s2 ->
        match skind s1, skind s2 with
        | S_add_marker m1, S_add_marker m2 ->
          compare_marker m1 m2
        | _ ->
          next s1 s2
      );
    visit = (fun next stmt ->
        match skind stmt with
        | S_add_marker _ -> leaf stmt
        | _ -> next stmt
      );
  }
