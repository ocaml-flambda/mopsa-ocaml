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

(** Zones for decomposing the AST into sub-languages. *)

open Ast

let debug fmt = Debug.debug ~channel:"framework.zone" fmt


(** {2 Zones}
    =========

    Zones define sub-languages in an analysis. They are used as
    arguments of the transfer functions [exec] and [eval] in order to
    filter appropriate domains to be activated.

    To simplify the processing of expressions/statements within a zone,
    generic eval templates can be defined. These templates are
    overrided by domains when necessary, and therefore they are used as
    fallbacks when no domain returns an answer.
*)


type zone = ..

type zone +=
  | Z_any            (** matches any defined zone *)
  | Z_under of zone  (** matches any sub-zone *)
  | Z_above of zone  (** matches any sup-zone *)

let any_zone = Z_any
let under_zone z = Z_under z
let above_zone z = Z_above z

let rec compare_zone (z1: zone) (z2: zone) : int =
  match z1, z2 with
  | Z_under z1, Z_under z2 -> compare_zone z1 z2
  | Z_above z1, Z_above z2 -> compare_zone z1 z2
  | _ -> Pervasives.compare z1 z2

let compare_zone2 = Compare.pair compare_zone compare_zone

type path = (zone * zone) list

let compare_eval_path (p1: path) (p2: path) =
  Compare.list compare_zone2 p1 p2

(* Registration of new zones *)
(* ------------------------- *)

type zone_info = {
  zone   : zone;
  subset : zone option;
  name   : string;
  eval   : expr -> action;
}

and action =
  | Keep
  | Visit
  | Process

(* Map pairing zones with their definitions *)
module ZoneMap = Map.Make(struct type t = zone let compare = compare_zone end)

let zones : zone_info ZoneMap.t ref = ref ZoneMap.empty

let register_zone info =
  zones := ZoneMap.add info.zone info !zones;
  ()

(* Matching predicates *)
(* ------------------- *)

let rec subset (z1: zone) (z2: zone) : bool =
  match z1, z2 with
  | z1, z2 when compare_zone z1 z2 = 0 -> true

  | Z_under z1', z2 -> subset z1' z2
  | z1, Z_above z2' -> subset z1 z2'

  | z1, z2 ->
    try
      let info = ZoneMap.find z1 !zones in
      match info.subset with
      | Some z -> subset z z2
      | None -> false
    with Not_found -> false


let rec sat_zone export z =
  match export, z with
  | Z_any, _ | _, Z_any -> true

  | Z_under z1, z2 -> subset z2 z1
  | z1, Z_under z2 -> subset z1 z2

  | Z_above z1, z2 -> subset z1 z2
  | z1, Z_above z2 -> subset z2 z1

  | _ -> compare_zone export z = 0

let sat_zone2 export zz =
  sat_zone (fst export) (fst zz) &&
  sat_zone (snd export) (snd zz)


(* Pretty printers *)
(* --------------- *)

let rec pp_zone fmt (z: zone) =
  match z with
  | Z_any -> Format.fprintf fmt "*"
  | Z_under z -> Format.fprintf fmt "↓ %a" pp_zone z
  | Z_above z -> Format.fprintf fmt "↑ %a" pp_zone z
  | _ ->
    try
      let info = ZoneMap.find z !zones in
      Format.pp_print_string fmt info.name
    with Not_found ->
      Exceptions.panic "Unknown zone"

let pp_zone2 fmt (z1, z2) = Format.fprintf fmt "[%a ↠ %a]" pp_zone z1 pp_zone z2

let pp_eval_path fmt (zp:path) =
  let rec aux fmt =
    function
    | [] -> ()
    | (z1, z2) :: tl -> Format.fprintf fmt " ↠ %a%a" pp_zone2 (z1, z2) aux tl
  in
  match zp with
  | [] -> ()
  | (z1, z2) :: tl ->  Format.fprintf fmt "%a%a" pp_zone2 (z1, z2) aux tl


(** {2 Static templates for evaluations}
    ====================================

    A zone may define a static template for evaluating an expression:

    a. The template evaluation can say that an expression is already
   in the target zone, which saves from iterating over domains.

    b. It can also say that an expression needs to be processed by
   domains.

    c. Finally, it can say that a visitor can be used to decompose the
   expression, evaluate each sub-expression and re-build the
   expression from the resulting sub-evaluations. This choice is used
   only when no domain answered the evaluation.

*)

let eval exp z =
  let template =
    try
      let info = ZoneMap.find z !zones in
      info.eval
    with Not_found ->
      (* Default template: call eval of domains *)
      (fun exp -> Process)
  in

  (* Evaluate expression by structural induction *)
  let rec aux exp =
    match template exp with
    | Keep -> Keep
    | Process -> Process
    | Visit ->
      (* Check whether all sub-expressions are part of the zone *)
      let evals =
        Visitor.fold_expr
          (fun acc exp ->
             VisitParts ((template exp) :: acc)
          )
          (fun acc stmt ->
             VisitParts (Process :: acc)
          )
          [] exp
      in
      if List.for_all (function Keep -> true | _ -> false) evals
      then Keep
      else Visit
  in
  aux exp


(** {2 Eval graph}
    ==============

    The collection of evaluations exported by domains is represented
    by a directed weighted graph. The vertices of the graph is the set
    of zones. An edge (z1, z2, w) represents an evaluation from zone z1
    to zone z2. The weight w represents the priority of the evaluation
    by considering the order of domains in the configuration file.
*)

module AdjencyMap = MapExt.Make(struct
    type t = zone
    let compare = compare_zone
  end)

module NextSet = Set.Make(struct
    type t = zone (* next vertex *) * int (* weight of the evaluation *)
    let compare = Compare.pair compare_zone compare
  end)

module ZoneSet = Set.Make(struct
    type t = zone
    let compare = compare_zone
  end)

type graph = {
  adjency: NextSet.t AdjencyMap.t;
  vertices: ZoneSet.t;
}

(* Pretty printers *)
(* --------------- *)

let pp_graph fmt g =
  let pp_set fmt s =
    Format.fprintf fmt "{ %a }"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       pp_zone
    ) (ZoneSet.elements s)
  in
  let pp_next fmt s =
    Format.fprintf fmt "{ %a }"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt (z, w) -> Format.fprintf fmt "%a:%d" pp_zone z w)
    ) (NextSet.elements s)
  in
  let pp_edges fmt adjency =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
      (fun fmt (z, next) -> Format.fprintf fmt "%a -> %a" pp_zone z pp_next next)
      fmt (AdjencyMap.bindings adjency)
  in
  Format.fprintf fmt "vertices : @[%a@]@\nedges    : @[%a@]"
    pp_set g.vertices
    pp_edges g.adjency

(* Creation of the eval graph *)
(* -------------------------- *)

let build_eval_graph (edges: (zone * zone) list) : graph =
  let g = {
    adjency  = AdjencyMap.empty;
    vertices = ZoneSet.empty;
  }
  in
  (* Give weights to edges; we assume that the order of edges follows
     the desired priority of evaluations *)
  List.mapi (fun w e -> w, e) edges |>
  List.fold_left (fun g (w, (z1, z2)) ->
      let succ = try AdjencyMap.find z1 g.adjency with Not_found -> NextSet.empty in
      {
        adjency  = AdjencyMap.add z1 (NextSet.add (z2, w) succ) g.adjency;
        vertices = ZoneSet.(g.vertices |> add z1 |> add z2);
      }
    ) g

let find_all_eval_paths (src:zone) (dst:zone) (g:graph) : path list =
  let rec dfs z before =
    let similar = ZoneSet.filter (fun z' -> sat_zone z z') g.vertices in
    ZoneSet.fold (fun zz acc ->
        let next = try AdjencyMap.find zz g.adjency with Not_found -> NextSet.empty in
        NextSet.fold (fun (z', w) acc ->
            if List.exists (fun (z1, z2, _) -> compare_zone zz z1 = 0 &&
                                               compare_zone z' z2 = 0
                           ) before
            then acc
            else
            if sat_zone z' dst
            then (before @ [(zz, z', w)]) :: acc
            else (dfs z' (before @ [(zz, z', w)])) @ acc
          ) next acc
        ) similar []
  in
  let weighted_paths = dfs src [] in
  (* Sort paths *)
  weighted_paths
  |>
  List.sort (fun wp1 wp2 ->
      Compare.list (fun (_, _, w1) (_, _, w2) -> compare w1 w2) wp1 wp2
    )
  |>
  List.map (List.map (fun (z1, z2, _) -> (z1, z2)))
  |>
  (* Remove duplicates *)
  (fun paths ->
     let rec aux =
       function
       | [] -> []
       | hd :: tl ->
         if List.exists (fun p -> compare_eval_path p hd = 0) tl
         then aux tl
         else hd :: aux tl
     in
     List.rev paths |>
     aux |>
     List.rev
  )
