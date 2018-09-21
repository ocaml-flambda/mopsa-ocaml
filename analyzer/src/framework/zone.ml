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

let debug fmt = Debug.debug ~channel:"framework.zone" fmt

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

let subset_chain : (zone -> zone -> bool) ref = ref (
    fun z1 z2 ->
      match z1, z2 with
      | Z_under _, _ | _, Z_under _
      | Z_above _, _ | _, Z_above _ -> assert false
      | _ -> z1 = z2
  )

let subset (z1: zone) (z2: zone) = !subset_chain z1 z2


let rec pp_chain : (Format.formatter -> zone -> unit) ref = ref (fun fmt zone ->
    match zone with
    | Z_any -> Format.fprintf fmt "∗"
    | Z_under z -> Format.fprintf fmt "↓%a" pp_zone z
    | Z_above z -> Format.fprintf fmt "↑%a" pp_zone z
    | _ -> failwith "Pp: Unknown zone"
  )

and pp_zone fmt (zone: zone) = Format.fprintf fmt "%a" !pp_chain zone

let pp_zone2 fmt (z1, z2) = Format.fprintf fmt "%a ↠ %a" pp_zone z1 pp_zone z2

type action =
  | Keep
  | Visit
  | Process

type zone_info = {
  zone            : zone;
  subset          : zone option;
  name            : string;
  exec_template   : Ast.stmt -> action;
  eval_template   : Ast.expr -> action;
}

let register_zone info =
  subset_chain := (
    match info.subset with
    | None -> !subset_chain
    | Some z ->  (fun z1 z2 ->
        if z1 = info.zone && z2 = z then true else
        if z1 = z && z2 = info.zone then false
        else !subset_chain z1 z2
      )
  );

  pp_chain := (fun fmt z ->
      if z = info.zone then Format.pp_print_string fmt info.name
      else !pp_chain fmt z
    );
  ()

let sat_zone export z =
  match z with
  | Z_any -> true
  | Z_under z' -> subset export z
  | Z_above z' -> subset z export
  | _ -> export = Z_any || z = export

let sat_zone2 export zz =
  sat_zone (fst export) (fst zz) && sat_zone (snd export) (snd zz)
