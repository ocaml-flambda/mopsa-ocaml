(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones define boundaries of sub-languages in an analysis. They are
   used as arguments of the transfer functions [exec], [eval], and
   [ask] in order to filter appropriate domains to use.
*)

let debug fmt = Debug.debug ~channel:"framework.zone" fmt

(*==========================================================================*)
                           (** {2 Zones} *)
(*==========================================================================*)

(** Zones are defined by languages extensions.*)
type zone = ..

type zone +=
  | Z_top (** ⊤ includes all languages of the analyzer *)

let top_zone = Z_top

(** Compare two zones.  Zones are assumed to be plain variant types,
   so there is no need to define a (total) compare function.  *)
let compare_zone (z1: zone) (z2: zone) : int = Pervasives.compare z1 z2

type info = {
  subset : (zone -> zone -> bool) -> zone -> zone -> bool;
  print  : (Format.formatter -> zone -> unit) -> Format.formatter -> zone -> unit;
}

(* Chain of partial order definitions. *)
let subset_zone_chain : (zone -> zone -> bool) ref = ref (
    fun z1 z2 ->
      match z1, z2 with
      | _, Z_top -> true
      | Z_top, _ -> false
      | _ -> z1 = z2
  )

let pp_zone_chain : (Format.formatter -> zone -> unit) ref = ref (fun fmt zone ->
    match zone with
    | Z_top -> Format.fprintf fmt "⊤"
    | _ -> failwith "Pp: Unknown zone"
  )

let register_zone info =
  subset_zone_chain := info.subset !subset_zone_chain;
  pp_zone_chain := info.print !pp_zone_chain;
  ()


let pp_zone fmt (zone: zone) =
  Format.fprintf fmt "[%a]" !pp_zone_chain zone

(** Partial order test. *)
let subset_zone (z1: zone) (z2: zone) =
  debug "%a subset %a" pp_zone z1 pp_zone z2;
  let b = !subset_zone_chain z1 z2 in
  debug "result: %b" b;
  b


(*==========================================================================*)
                           (** {2 Paths} *)
(*==========================================================================*)


(** Compare two paths. *)
let compare_zone2 (z1, z2) (z1', z2') =
  let c1 = compare z1 z1' in
  if c1 <> 0 then c1
  else compare z2 z2'

let pp_zone2 fmt (z1, z2) =
  Format.fprintf fmt "%a -> %a" pp_zone z1 pp_zone z2

(** (z1, z2) ⊆ (z1', z2') iff. z1 ⊆ z1' and z2 ⊆ z2'. *)
let subset_zone2 (z1, z2) (z1', z2') =
  subset_zone z1 z1' && subset_zone z2 z2'

