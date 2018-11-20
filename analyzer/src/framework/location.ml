(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Locations and ranges *)


(** {2 Locations} *)
(** ============= *)

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



(** {2 Ranges} *)
(** ========== *)


(** Location range of an AST node. *)

type range_tag = string
(** Range tags can be used to annotate AST nodes added by the abstract domains,
    that are not textually present in the source files. *)

(** Location range of AST nodes. *)
type range =
  | R_program of string list (** list of source files *)
  (** Program range covering a list source files *)

  | R_source of loc (** start location. *) * loc (** end location *)
  (** Source range with a start and an end locations. *)

  | R_fresh of int (** non-original fresh range with unique id *)
  (** Fresh ranges with unique identifiers *)
      
  | R_tagged of string * range
  (** Tagged range with a string annotation *)

(** Tag a range with a (formatted) annotation. *)
let tag_range range fmt =
  Format.kasprintf (fun tag ->
      R_tagged (tag, range)
    ) fmt

let mk_source_range loc1 loc2 = R_source (loc1, loc2)

let fresh_range_counter = ref 0

let mk_fresh_range () =
  incr fresh_range_counter;
  R_fresh !fresh_range_counter

let mk_program_range pl = R_program pl

let rec untag_range = function
  | R_tagged(_, range) -> untag_range range
  | range -> range

let rec map_tag f = function
  | R_tagged (t, r) -> R_tagged (t, map_tag f r)
  | r -> f r
                            
let get_range_file r =
  match untag_range r with
  | R_program [f] -> f
  | R_program _ -> Debug.fail "get_range_file: wrong program range"
  | R_source (loc1, _) -> loc1.loc_file
  | R_fresh _ -> Debug.fail "get_range_file: called on R_fresh"
  | R_tagged _ -> assert false

let get_range_line r =
  match untag_range r with
  | R_source (loc1, _) -> loc1.loc_line
  | _ -> Debug.fail "get_range_line: called on non R_source"

let get_range_start r =
  match untag_range r with
  | R_source (l, _) -> l
  | _ -> Debug.fail "get_range_start: called on non R_source"

let get_range_end r =
  match untag_range r with
  | R_source (_, l) -> l
  | _ -> Debug.fail "get_range_end: called on non R_source"

let set_range_start r l =
  map_tag (fun r ->
      match r with
      | R_source (_, l') -> R_source (l, l')
      | _ -> Debug.fail "set_range_start: called on non R_source"
    ) r

let set_range_end r l' =
  map_tag (fun r ->
      match r with
      | R_source (l, _) -> R_source (l, l')
      | _ -> Debug.fail "set_range_end: called on non R_source"
    ) r


(** Comparison function of ranges. *)
let rec compare_range (r1: range) (r2: range) =
  match r1, r2 with
  | R_program pl1, R_program pl2 ->
    Compare.list compare pl1 pl2

  | R_source (l1, l2), R_source (l1', l2') ->
    Compare.compose [
      (fun () -> compare_location l1 l2);
      (fun () -> compare_location l1' l2');
    ]

  | R_tagged(t1, r1), R_tagged(t2, r2) ->
    Compare.compose [
      (fun () -> compare_range r1 r2);
      (fun () -> compare t1 t2)
    ]

  | R_fresh(uid1), R_fresh(uid2) -> compare uid1 uid2

  | _ -> compare r1 r2



(** {2 Pretty printers} *)
(** =================== *)

let rec pp_location fmt loc =
  Format.fprintf fmt "%d:%d" loc.loc_line loc.loc_column

and pp_location_verbose fmt loc =
  Format.fprintf fmt "file %s:%d"
    loc.loc_file loc.loc_line

and pp_range fmt range =
  match range with
  | R_program pl -> Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_string fmt pl
  | R_source (l1, l2) -> Format.fprintf fmt "%a-%a" pp_location l1 pp_location l2
  | R_fresh uid -> Format.fprintf fmt "!%d" uid
  | R_tagged (t, r) -> Format.fprintf fmt "%a<%s>" pp_range r t

and pp_range_verbose fmt range =
  match range with
  | R_program pl -> pp_range fmt range
  | R_source (l1, _) -> pp_location_verbose fmt l1
  | R_fresh uid -> pp_range fmt range
  | R_tagged(t, r) -> Format.fprintf fmt "%a~%s" pp_range_verbose r t
