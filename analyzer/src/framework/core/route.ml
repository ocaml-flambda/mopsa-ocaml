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

open Semantic

type domain = string

type route =
  | Below
  | BelowOf of domain
  | Semantic of semantic

let compare_route r1 r2 =
  match r1, r2 with
  | Below, Below -> 0
  | BelowOf dom1, BelowOf dom2 -> String.compare dom1 dom2
  | Semantic sem1, Semantic sem2 -> String.compare sem1 sem2
  | _ -> compare r1 r2

let pp_route fmt = function
  | Below        -> Format.fprintf fmt "below"
  | BelowOf dom  -> Format.fprintf fmt "below(%s)" dom
  | Semantic sem -> pp_semantic fmt sem


let toplevel = Semantic toplevel_semantic

module Map =
  MapExt.Make
    (struct
      type t = route
      let compare = compare_route
    end)

type routing_table = domain list Map.t

let empty_routing_table = Map.empty

let resolve_route route map = Map.find route map

let add_route selector domain map =
  let old = try Map.find selector map with Not_found -> [] in
  Map.add selector (domain :: old) map

let add_routes selector domains map =
  let old = try Map.find selector map with Not_found -> [] in
  Map.add selector (domains @ old) map

let get_routes map = Map.bindings map |> List.map fst

let join_routing_table m1 m2 =
  Map.map2zo
    (fun sem1 domains1 -> domains1)
    (fun sem2 domains2 -> domains2)
    (fun sem domains1 domains2 -> domains1 @ domains2)
    m1 m2

let pp_routing_table fmt m =
  Format.(fprintf fmt "@[<v>%a@]"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
               (fun fmt (route,domains) ->
                  fprintf fmt "%a -> {%a}"
                    pp_route route
                    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_print_string) domains
               )
            ) (Map.bindings m)
         )
                       
