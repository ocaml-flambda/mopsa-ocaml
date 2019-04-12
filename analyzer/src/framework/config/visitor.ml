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
  seq : json list -> 'a;
  compose : json list -> 'a;
  apply : json -> json -> 'a;
  nonrel : json -> 'a;
  product : json list -> string list -> 'a;
}

let rec visit visitor json =
  match json with
  | `String s -> visit_leaf visitor s
  | `Assoc obj when List.mem_assoc "seq" obj -> visit_seq visitor obj
  | `Assoc obj when List.mem_assoc "compose" obj -> visit_compose visitor obj
  | _ -> Exceptions.panic "parsing error: configuration not supported@  %a"
           (pretty_print ~std:true) json

and visit_leaf visitor s = visitor.leaf s

and visit_seq visitor obj =
  let l = List.assoc "seq" obj |> to_list in
  visitor.seq l

and visit_compose visitor obj =
  let l = List.assoc "compose" obj |> to_list in
  visitor.compose l
