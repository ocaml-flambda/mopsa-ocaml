(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones define sub-languages in an analysis. They are used as
   arguments of the transfer functions [exec] and [eval] in order to
   filter appropriate domains to be activated.

   To simplify the processing of expressions/statements within a zone,
   generic eval templates can be defined. These templates are
   overrided by domains when necessary, and therefore they are used as
   fallbacks when no domain returns an answer.
*)

open Ast

let debug fmt = Debug.debug ~channel:"framework.zone" fmt

type zone = ..

type zone +=
  | Z_any            (** matches any defined zone *)
  | Z_under of zone  (** matches any sub-zone *)
  | Z_above of zone  (** matches any sup-zone *)

let any_zone = Z_any
let under_zone z = Z_under z
let above_zone z = Z_above z


(** {2 Registration of new zones} *)
(** ============================= *)

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


let rec compare_zone (z1: zone) (z2: zone) : int =
  match z1, z2 with
  | Z_under z1, Z_under z2 -> compare_zone z1 z2
  | Z_above z1, Z_above z2 -> compare_zone z1 z2
  | _ -> Pervasives.compare z1 z2

let compare_zone2 = Compare.pair compare_zone compare_zone

(* Map pairing zones with their definitions *)
module ZoneMap = Map.Make(struct type t = zone let compare = compare_zone end)

let zones : zone_info ZoneMap.t ref = ref ZoneMap.empty

let register_zone info =
  zones := ZoneMap.add info.zone info !zones;
  ()


(** {2 Comparison predicates} *)
(** ========================= *)

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


let sat_zone export z =
  match z with
  | Z_any -> true
  | Z_under z' -> subset export z
  | Z_above z' -> subset z export
  | _ -> compare_zone export Z_any = 0 ||
         compare_zone export z = 0

let sat_zone2 export zz =
  sat_zone (fst export) (fst zz) &&
  sat_zone (snd export) (snd zz)


(** {2 Pretty printers} *)
(** =================== *)

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
      Debug.fail "Unknown zone"

let pp_zone2 fmt (z1, z2) = Format.fprintf fmt "[%a ↠ %a]" pp_zone z1 pp_zone z2

let pp_zone_path fmt zp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt " ↠ ")
    pp_zone fmt zp

(** {2 Static evaluation of expressions} *)
(** ==================================== *)

let eval exp z =
  let template =
    try
      let info = ZoneMap.find z !zones in
      info.eval
    with Not_found ->
      (fun exp -> Process)
  in

  let rec aux exp =
    match template exp with
    | Keep -> Keep
    | Process -> Process
    | Visit ->
      let evals, is_stmt_free =
        Visitor.fold_expr
          (fun (eacc, sacc) exp ->
             (template exp) :: eacc, sacc
          )
          (fun (eacc, sacc) stmt ->
             eacc, false
          )
          ([], true) exp
      in
      if is_stmt_free then
        if List.for_all (function Keep -> true | _ -> false) evals then
          Keep
        else
          Visit
      else
        Process
  in

  aux exp


(** {2 Zoning graph} *)
(** ================ *)

module AdjencyMap = MapExt.Make(struct
    type t = zone
    let compare = compare_zone
  end)

module ZoneSet = Set.Make(struct
    type t = zone
    let compare = compare_zone
  end)

type graph = {
  adjency: ZoneSet.t AdjencyMap.t;
  vertices: ZoneSet.t;
}

let pp_graph fmt g =
  let pp_set fmt s =
    Format.fprintf fmt "{ %a }"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       pp_zone
    ) (ZoneSet.elements s)
  in
  let pp_edges fmt adjency =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
      (fun fmt (z, next) -> Format.fprintf fmt "%a -> %a" pp_zone z pp_set next)
      fmt (AdjencyMap.bindings adjency)
  in
  Format.fprintf fmt "vertices : @[%a@]@\nedges    : @[%a@]"
    pp_set g.vertices
    pp_edges g.adjency

let build_zoning_graph (edges: (zone * zone) list) : graph =
  let g = {
    adjency  = AdjencyMap.empty;
    vertices = ZoneSet.empty;
  }
  in
  edges |>
  List.fold_left (fun g (z1, z2) ->
      debug "adding edge %a" pp_zone2 (z1, z2);
      let succ = try AdjencyMap.find z1 g.adjency with Not_found -> ZoneSet.empty in
      {
        adjency  = AdjencyMap.add z1 (ZoneSet.add z2 succ) g.adjency;
        vertices = ZoneSet.(g.vertices |> add z1 |> add z2);
      }
    ) g

let find_all_paths (src:zone) (dst:zone) (g:graph) : zone list list =
  let rec bfs z before =
    let next = try AdjencyMap.find z g.adjency with Not_found -> ZoneSet.empty in
    ZoneSet.fold (fun z' acc ->
        if List.exists (fun z'' -> compare_zone z' z'' = 0) before then acc
        else if sat_zone z' dst then (before @ [z']) :: acc
        else (bfs z' (before @ [z'])) @ acc
      ) next []
  in
  ZoneSet.fold (fun z acc ->
      if sat_zone z src
      then bfs z [z] @ acc
      else acc
    ) g.vertices []

