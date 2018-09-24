(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones define boundaries of sub-languages in an analysis. They are
   used as arguments of the transfer functions [exec] and [eval] in
   order to filter appropriate domains to use.

   To simplify the processing of expressions/statements within a zone,
   generic templates for [eval] and [exec] can be defined. These
   templates are overrided by domains when necessary, and therefore
   they are used as fallback when no domain return an answer.

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
  | _ -> export = Z_any || subset export z

let sat_zone2 export zz =
  sat_zone (fst export) (fst zz) && sat_zone (snd export) (snd zz)



(** {2 Pretty printers} *)
(** =================== *)

let rec pp_zone fmt (z: zone) =
  match z with
  | Z_any -> Format.fprintf fmt "∗ "
  | Z_under z -> Format.fprintf fmt "↓ %a" pp_zone z
  | Z_above z -> Format.fprintf fmt "↑ %a" pp_zone z
  | _ ->
    try
      let info = ZoneMap.find z !zones in
      Format.pp_print_string fmt info.name
    with Not_found ->
      Debug.fail "Unknown zone"

let pp_zone2 fmt (z1, z2) = Format.fprintf fmt "[%a ↠ %a]" pp_zone z1 pp_zone z2


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
