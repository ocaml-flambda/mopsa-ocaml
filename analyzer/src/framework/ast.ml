(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)



(** Extensible Abstract Syntax Tree. *)


(**
   [compare_composer cl] applies a list of comparison functions [cl] in sequence
   and stops when encoutring the first non-zero result.
*)
let rec compare_composer = function
  | [] -> 0
  | cmp :: tl ->
    let r = cmp () in
    if r <> 0 then r else compare_composer tl


(*==========================================================================*)
                 (**     {2 Locations and ranges}      *)
(*==========================================================================*)

type loc = {
  loc_file: string; (** Filename. *)
  loc_line: int; (** Line number. *)
  loc_column: int; (** Column number. *)
}
(** Location of an AST node. *)

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

let mk_loc file line column =
  {loc_file = file; loc_line = line; loc_column = column}

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

(** Comparison function of locations. *)
let compare_location (l1: loc) (l2: loc) =
  compare_composer [
    (fun () -> compare l1.loc_file l2.loc_file);
    (fun () -> compare l1.loc_line l2.loc_line);
    (fun () -> compare l1.loc_column l2.loc_column);
  ]

(** Comparison function of ranges. *)
let rec compare_range (r1: range) (r2: range) =
  match r1, r2 with
  | Range_origin r1, Range_origin r2 ->
    compare_composer [
      (fun () -> compare_location r1.range_begin r2.range_begin);
      (fun () -> compare_location r1.range_end r2.range_end);
    ]
  | Range_file f1, Range_file f2 -> compare f1 f2
  | Range_tagged(t1, r1), Range_tagged(t2, r2) ->
    compare_composer [
      (fun () -> compare_range r1 r2);
      (fun () -> compare t1 t2)
    ]
  | Range_fresh(uid1), Range_fresh(uid2) -> compare uid1 uid2
  | _ -> compare r1 r2


(*==========================================================================*)
                        (** {2 Programs} *)
(*==========================================================================*)
type program_kind = ..
(** Extensible type for describing analyzed programs. *)

type program = {
  prog_kind : program_kind;
  prog_file : string;
}

(*==========================================================================*)
                     (**      {2 Statements}      *)
(*==========================================================================*)

type stmt_kind = ..
(** Extensible statements kinds. *)

type stmt_kind +=
  | S_program of program
  (** Program to be analyzed *)

type stmt = {
  skind : stmt_kind; (** Kind of the statement. *)
  srange : range; (** Location range of the statement. *)
}
(** Statements with their kind and range. *)

let skind (stmt: stmt) = stmt.skind
let srange (stmt: stmt) = stmt.srange
let mk_stmt skind srange =
  {skind; srange}



(*==========================================================================*)
                      (**      {2 Types}      *)
(*==========================================================================*)

type typ = ..
(** Extensible type of expression types. *)

type typ +=
  | T_any (** Generic unknown type. *)


(*==========================================================================*)
                      (**      {2 Variables}      *)
(*==========================================================================*)

type var = {
  vname : string; (** original name of the variable. *)
  vuid : int; (** unique identifier. *)
  vtyp : typ; (** type of the variable. *)
  vkind : var_kind; (** extra kind of the variable. *)
}
(** variables *)

and var_kind = ..

type var_kind +=
  | V_orig (** original program variables. *)

let vkind v = v.vkind

let var_uniq_name v =
  if v.vuid <= 0 then
    v.vname
  else
    v.vname ^ "@" ^ (string_of_int v.vuid)

let var_compare_chain : (var -> var -> int) ref = ref (fun v1 v2 ->
    match vkind v1, vkind v2 with
    | V_orig, V_orig -> 0
    | _ -> compare v1 v2
  )

let register_var_compare cmp =
  var_compare_chain := cmp !var_compare_chain

let compare_var v1 v2 =
  compare_composer [
    (fun () -> compare v1.vname v2.vname);
    (fun () -> compare v1.vuid v2.vuid);
    (fun () -> compare v1.vtyp v2.vtyp);
    (fun () -> !var_compare_chain v1 v2);
  ]

let mkv ?(vtyp = T_any) ?(vkind = V_orig) ?(vuid=0) vname =
  {vname; vuid = 0; vtyp; vkind}

let tmp_counter = ref 0

(** Create a temporary variable with a unique name. *)
let mktmp ?(vtyp = T_any) ?(vkind = V_orig) () =
  incr tmp_counter;
  let vname = "$tmp" ^ (string_of_int !tmp_counter) in
  {vname; vuid = 0; vtyp; vkind}


(*==========================================================================*)
                      (**      {2 Expressions}      *)
(*==========================================================================*)


type operator = ..
(** Extensible type of operators (unary, binary, etc.). *)

type constant = ..
(** Extensible type of constants. *)

type expr = {
  ekind: expr_kind;
  etyp: typ;
  erange: range;
}

(** Extensible type of expressions. *)
and expr_kind = ..

type expr_kind +=
  | E_var of var (** variables *)
  | E_constant of constant (** constants *)
  | E_unop of operator * expr (** unary operator expressions *)
  | E_binop of operator * expr * expr (** binary operator expressions *)

(** Type-decorated expressions. *)

let ekind (e: expr) = e.ekind
let etyp (e: expr) = e.etyp
let erange (e: expr) = e.erange
let mk_expr
    ?(etyp = T_any)
    ekind
    erange
  =
  {ekind; etyp; erange}

let mk_var v erange =
  mk_expr ~etyp:v.vtyp (E_var v) erange

let mk_binop left op right ?(etyp = T_any) erange =
  mk_expr (E_binop (op, left, right)) ~etyp erange

let mk_unop op operand erange =
  mk_expr (E_unop (op, operand)) erange

let mk_constant ~etyp c = mk_expr ~etyp (E_constant c)
