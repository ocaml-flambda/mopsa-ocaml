(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Locations and ranges *)


(*==========================================================================*)
(**                           {2 Locations}                                 *)
(*==========================================================================*)

type loc = {
  loc_file: string; (** Filename. *)
  loc_line: int; (** Line number. *)
  loc_column: int; (** Column number. *)
}
(** Location of an AST node. *)


let mk_loc file line column =
  {loc_file = file; loc_line = line; loc_column = column}

(** Comparison function of locations. *)
let compare_location (l1: loc) (l2: loc) =
  Compare.compose [
    (fun () -> compare l1.loc_file l2.loc_file);
    (fun () -> compare l1.loc_line l2.loc_line);
    (fun () -> compare l1.loc_column l2.loc_column);
  ]


(*==========================================================================*)
(**                            {2 Ranges}                                   *)
(*==========================================================================*)


type range_orig = {
  range_begin: loc; (** Start location. *)
  range_end: loc; (** End location. *)
}
(** Location range of an AST node. *)

type range_tag = string
(** Range tags can be used to annotate AST nodes added by the abstract domains,
    that are not textually present in the source files. *)

(** Location range of AST nodes. *)
type range =
  | Range_file of string (** range covering an entire file *)
  | Range_origin of range_orig (** original range from source files *)
  | Range_fresh of int (** non-original fresh range with unique id *)
  | Range_tagged of range_tag * range

(** Tag a range with a (formatted) annotation. *)
let tag_range range fmt =
  Format.kasprintf (fun tag ->
      Range_tagged (tag, range)
    ) fmt


let mk_range loc1 loc2 =
  {range_begin = loc1; range_end = loc2}

let fresh_range_counter = ref 0

let mk_fresh_range () =
  incr fresh_range_counter;
  Range_fresh !fresh_range_counter

let mk_file_range f = Range_file f

let rec get_origin_range = function
  | Range_origin range -> range
  | Range_file filename -> mk_range (mk_loc filename 1 1) (mk_loc filename (-1) (-1))
  | Range_tagged(_, range) -> get_origin_range range
  | Range_fresh _ -> failwith "get_origin_range: call on non-original range"

let range_begin r = r.range_begin

(** Comparison function of ranges. *)
let rec compare_range (r1: range) (r2: range) =
  match r1, r2 with
  | Range_origin r1, Range_origin r2 ->
    Compare.compose [
      (fun () -> compare_location r1.range_begin r2.range_begin);
      (fun () -> compare_location r1.range_end r2.range_end);
    ]
  | Range_file f1, Range_file f2 -> compare f1 f2
  | Range_tagged(t1, r1), Range_tagged(t2, r2) ->
    Compare.compose [
      (fun () -> compare_range r1 r2);
      (fun () -> compare t1 t2)
    ]
  | Range_fresh(uid1), Range_fresh(uid2) -> compare uid1 uid2
  | _ -> compare r1 r2


(*==========================================================================*)
(**                      {2 Pretty printers}                                *)
(*==========================================================================*)


let rec pp_location fmt loc =
  Format.fprintf fmt "%d:%d" loc.loc_line loc.loc_column

and pp_location_verbose fmt loc =
  Format.fprintf fmt "file %s:%d"
    loc.loc_file loc.loc_line

and pp_range fmt range =
  match range with
  | Range_origin r ->
    Format.fprintf fmt "%a-%a"
      pp_location r.range_begin pp_location r.range_end
  | Range_file f ->
    Format.fprintf fmt "%s" f
  | Range_fresh uid ->
    (* Format.fprintf fmt "!%d" uid *)
    ()
  | Range_tagged (t, r) ->
    Format.fprintf fmt "%a<%s>" pp_range r t

and pp_range_verbose fmt range =
  match range with
  | Range_origin r ->
    pp_location_verbose fmt r.range_begin
  | Range_file f ->
    Format.fprintf fmt "File %s" f
  | Range_fresh uid ->
    Format.fprintf fmt "!%d" uid
  | Range_tagged(t, r) ->
    Format.fprintf fmt "%a<%s>" pp_range_verbose r t
