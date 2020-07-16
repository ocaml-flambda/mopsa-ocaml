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


(** Semantic routes *)

open Semantic

module Map =
  MapExt.Make
    (struct
      type t = semantic
      let compare = compare_semantic
    end)

type domain = string

type wirings = domain list Map.t

let empty_wirings = Map.empty

let find_wirings semantic map = Map.find semantic map

let add_wiring semantic domain map =
  let old = try Map.find semantic map with Not_found -> [] in
  Map.add semantic (domain :: old) map

let add_wirings semantic domains map =
  let old = try Map.find semantic map with Not_found -> [] in
  Map.add semantic (domains @ old) map

let join_wirings m1 m2 =
  Map.map2zo
    (fun sem1 domains1 -> domains1)
    (fun sem2 domains2 -> domains2)
    (fun sem domains1 domains2 -> domains1 @ domains2)
    m1 m2

let pp_wirinings fmt m =
  Format.(fprintf fmt "@[<v>%a@]"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
               (fun fmt (semantic,domains) ->
                  fprintf fmt "%a -> {%a}"
                    pp_semantic semantic
                    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_string) domains
               )
            ) (Map.bindings m)
         )
                       
