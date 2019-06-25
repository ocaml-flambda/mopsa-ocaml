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

(** Visitor of configuration files *)


open Yojson.Basic
open Yojson.Basic.Util


type 'a visitor = {
  leaf : string -> 'a;
  seq : Yojson.Basic.t list -> 'a;
  compose : Yojson.Basic.t list -> 'a;
  disjoint : Yojson.Basic.t list -> 'a;
  apply : Yojson.Basic.t -> Yojson.Basic.t -> 'a;
  nonrel : Yojson.Basic.t -> 'a;
  product : Yojson.Basic.t list -> string list -> 'a;
}

let rec visit visitor json =
  match json with
  | `String s -> visit_leaf visitor s
  | `Assoc obj when List.mem_assoc "seq" obj -> visit_seq visitor obj
  | `Assoc obj when List.mem_assoc "compose" obj -> visit_compose visitor obj
  | `Assoc obj when List.mem_assoc "apply" obj -> visit_apply visitor obj
  | `Assoc obj when List.mem_assoc "nonrel" obj -> visit_nonrel visitor obj
  | `Assoc obj when List.mem_assoc "product" obj -> visit_product visitor obj
  | `Assoc obj when List.mem_assoc "disjoint" obj -> visit_disjoint visitor obj
  | _ -> Exceptions.panic "parsing error: configuration not supported@  %a"
           (pretty_print ~std:true) json

and visit_leaf visitor s = visitor.leaf s

and visit_seq visitor obj =
  let l = List.assoc "seq" obj |> to_list in
  visitor.seq l

and visit_compose visitor obj =
  let l = List.assoc "compose" obj |> to_list in
  visitor.compose l

and visit_apply visitor obj =
  let s = List.assoc "apply" obj in
  let d = List.assoc "on" obj in
  visitor.apply s d

and visit_nonrel visitor obj =
  let v = List.assoc "nonrel" obj in
  visitor.nonrel v

and visit_disjoint visitor obj =
  let l = List.assoc "disjoint" obj |> to_list in
  visitor.disjoint l

and visit_product visitor obj =
  let l = List.assoc "product" obj |> to_list in
  let r = try List.assoc "reductions" obj |> to_list |> List.map to_string with Not_found -> [] in
  visitor.product l r
