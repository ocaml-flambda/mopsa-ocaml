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

(** Concrete syntax tree for C stubs *)

open Location

type stub = section list with_range

(** {2 Stub sections} *)
(** ***************** *)

and section =
  | S_predicate of predicate with_range
  | S_case      of case with_range
  | S_leaf      of leaf
  | S_alias     of alias with_range

and leaf =
  | S_local     of local with_range
  | S_assumes   of assumes with_range
  | S_requires  of requires with_range
  | S_assigns   of assigns with_range
  | S_ensures   of ensures with_range
  | S_free      of free with_range
  | S_warn      of warn with_range

and case = {
  case_label     : string;
  case_body      : leaf list;
}

and alias = string

(** {2 Stub leaf sections} *)
(** ********************** *)

and requires = formula with_range

and ensures = formula with_range

and assumes = formula with_range

and local = {
  lvar   : var;
  ltyp   : c_qual_typ;
  lval : local_value;
}

and local_value =
  | L_new of resource
  | L_call  of var with_range (** function *) * expr with_range list (* arguments *)

and assigns = {
  assign_target : expr with_range;
  assign_offset : (expr with_range * expr with_range) list;
}

and free = expr with_range

and predicate = {
  predicate_var  : var;
  predicate_args : var list;
  predicate_body : formula with_range;
}

and warn = string

(** {2 Formulas} *)
(** ************ *)

and formula =
  | F_expr      of expr with_range
  | F_bool      of bool
  | F_binop     of log_binop * formula with_range * formula with_range
  | F_not       of formula with_range
  | F_forall    of var * c_qual_typ * set * formula with_range
  | F_exists    of var * c_qual_typ * set * formula with_range
  | F_in        of expr with_range * set
  | F_predicate of var * expr with_range list



(** {2 Expressions} *)
(** *************** *)

and expr =
  | E_int       of Z.t * int_suffix
  | E_float     of float
  | E_string    of string
  | E_char      of int
  | E_invalid

  | E_var       of var

  | E_unop      of unop  * expr with_range
  | E_binop     of binop * expr with_range * expr with_range

  | E_addr_of   of expr with_range
  | E_deref     of expr with_range
  | E_cast      of c_qual_typ * expr with_range

  | E_subscript of expr with_range * expr with_range
  | E_member    of expr with_range * string
  | E_attribute of expr with_range * string
  | E_arrow     of expr with_range * string

  | E_builtin_call  of builtin * expr with_range

  | E_sizeof_type   of c_qual_typ with_range
  | E_sizeof_expr   of expr with_range

  | E_return

and int_suffix =
  | NO_SUFFIX
  | LONG
  | UNSIGNED_LONG
  | LONG_LONG
  | UNSIGNED_LONG_LONG
    

and log_binop =
  | AND
  | OR
  | IMPLIES

and binop =
  | ADD     (* + *)
  | SUB     (* - *)
  | MUL     (* * *)
  | DIV     (* / *)
  | MOD     (* % *)
  | RSHIFT  (* >> *)
  | LSHIFT  (* << *)
  | LOR     (* || *)
  | LAND    (* && *)
  | LT      (* < *)
  | LE      (* <= *)
  | GT      (* > *)
  | GE      (* >= *)
  | EQ      (* == *)
  | NEQ     (* != *)
  | BOR     (* | *)
  | BAND    (* & *)
  | BXOR    (* ^ *)

and unop =
  | PLUS    (* + *)
  | MINUS   (* - *)
  | LNOT    (* ! *)
  | BNOT    (* ~ *)

and set =
  | S_interval of expr with_range * expr with_range
  | S_resource of resource

and resource = string

and var = {
  vname  : string;     (** variable name *)
  vlocal : bool;       (** is it a local variable ? *)
  vuid   : int;        (** unique identifier *)
  vtyp   : c_qual_typ; (** variable type *)
  vrange : range;      (** declaration location *)
}

and builtin =
  | PRIMED
  | SIZE
  | BYTES
  | OFFSET
  | BASE
  | VALID_PTR
  | VALID_FLOAT
  | FLOAT_INF
  | FLOAT_NAN


(** {2 Types} *)
(*  ********* *)


and c_qual_typ = c_typ * bool (** is const ? *)

and c_typ =
  | T_void
  | T_char
  | T_signed_char | T_unsigned_char
  | T_signed_short | T_unsigned_short
  | T_signed_int | T_unsigned_int
  | T_signed_long | T_unsigned_long
  | T_signed_long_long | T_unsigned_long_long
  | T_signed_int128 | T_unsigned_int128
  | T_float | T_double | T_long_double
  | T_array of c_qual_typ * array_length
  | T_struct of var
  | T_union of var
  | T_typedef of var
  | T_pointer of c_qual_typ
  | T_enum of var
  | T_unknown

and array_length =
  | A_no_length
  | A_constant_length of Z.t



(** {2 Utility functions} *)
(** ********************* *)

let compare_var v1 v2 =
  Compare.compose [
    (fun () -> compare v1.vname v2.vname);
    (fun () -> compare v1.vuid v2.vuid);
  ]

let compare_resource (r1:resource) (r2:resource) = compare r1 r2

let no_qual t = t, false

let is_alias (cst:stub) : bool =
  match cst.content with
  | [ S_alias _ ] -> true
  | _ -> false

(** {2 Pretty printers} *)
(** ******************* *)

open Format

let pp_var fmt v = pp_print_string fmt v.vname

let pp_resource fmt resource = pp_print_string fmt resource

let pp_builtin fmt f =
  match f with
  | BYTES   -> pp_print_string fmt "bytes"
  | SIZE   -> pp_print_string fmt "size"
  | OFFSET -> pp_print_string fmt "offset"
  | BASE   -> pp_print_string fmt "base"
  | PRIMED -> pp_print_string fmt "primed"
  | VALID_PTR -> pp_print_string fmt "valid_ptr"
  | VALID_FLOAT -> pp_print_string fmt "valid_float"
  | FLOAT_INF   -> pp_print_string fmt "float_inf"
  | FLOAT_NAN   -> pp_print_string fmt "float_nan"

let pp_list pp sep fmt l =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt sep) pp fmt l

let rec pp_expr fmt exp =
  match get_content exp with
  | E_int (n, suffix) -> fprintf fmt "%a%a" Z.pp_print n pp_int_suffix suffix
  | E_float f -> pp_print_float fmt f
  | E_string s -> fprintf fmt "\"%s\"" s
  | E_char c ->
    if c >= 32 && c < 127
    then fprintf fmt "'%c'" (Char.chr c)
    else fprintf fmt "%d" c
  | E_invalid -> fprintf fmt "INVALID"
  | E_var v -> pp_var fmt v
  | E_unop (op, e) -> fprintf fmt "%a %a" pp_unop op pp_expr e
  | E_binop (op, e1, e2) -> fprintf fmt "%a %a %a" pp_expr e1 pp_binop op pp_expr e2
  | E_addr_of e -> fprintf fmt "&%a" pp_expr e
  | E_deref e -> fprintf fmt "*%a" pp_expr e
  | E_cast(t, e) -> fprintf fmt "(%a) %a" pp_c_qual_typ t pp_expr e
  | E_subscript(a, i) -> fprintf fmt "%a[%a]" pp_expr a pp_expr i
  | E_member(s, f) -> fprintf fmt "%a.%s" pp_expr s f
  | E_attribute(s, f) -> fprintf fmt "%a:%s" pp_expr s f
  | E_arrow(p, f) -> fprintf fmt "%a->%s" pp_expr p f
  | E_builtin_call(f, arg) -> fprintf fmt "%a(%a)" pp_builtin f pp_expr arg
  | E_sizeof_type t -> fprintf fmt "sizeof(%a)" pp_c_qual_typ t.content
  | E_sizeof_expr e -> fprintf fmt "sizeof(%a)" pp_expr e
  | E_return -> pp_print_string fmt "return"

and pp_int_suffix fmt =
  function
  | NO_SUFFIX -> ()
  | LONG -> fprintf fmt "L"
  | UNSIGNED_LONG -> fprintf fmt "UL"
  | LONG_LONG -> fprintf fmt "LL"
  | UNSIGNED_LONG_LONG -> fprintf fmt "ULL"

and pp_unop fmt =
  function
  | PLUS    -> pp_print_string fmt "+"
  | MINUS   -> pp_print_string fmt "-"
  | LNOT    -> pp_print_string fmt "!"
  | BNOT    -> pp_print_string fmt "~"

and pp_binop fmt =
  function
  | ADD     -> pp_print_string fmt "+"
  | SUB     -> pp_print_string fmt "-"
  | MUL     -> pp_print_string fmt "*"
  | DIV     -> pp_print_string fmt "/"
  | MOD     -> pp_print_string fmt "%"
  | RSHIFT  -> pp_print_string fmt ">>"
  | LSHIFT  -> pp_print_string fmt "<<"
  | LOR     -> pp_print_string fmt "||"
  | LAND    -> pp_print_string fmt "&&"
  | LT      -> pp_print_string fmt "<"
  | LE      -> pp_print_string fmt "<="
  | GT      -> pp_print_string fmt ">"
  | GE      -> pp_print_string fmt ">="
  | EQ      -> pp_print_string fmt "=="
  | NEQ     -> pp_print_string fmt "!="
  | BOR     -> pp_print_string fmt "|"
  | BAND    -> pp_print_string fmt "&"
  | BXOR    -> pp_print_string fmt "^"

and pp_c_qual_typ fmt =
  function
  | (t, true) -> fprintf fmt "const %a" pp_c_typ t
  | (t, false) -> pp_c_typ fmt t

and pp_c_typ fmt =
  function
  | T_void -> pp_print_string fmt "void"
  | T_char -> pp_print_string fmt "char"
  | T_signed_char  -> pp_print_string fmt "signed char"
  | T_unsigned_char  -> pp_print_string fmt "unsigned char"
  | T_signed_short  -> pp_print_string fmt "signed short"
  | T_unsigned_short  -> pp_print_string fmt "unsigned short"
  | T_signed_int  -> pp_print_string fmt "signed int"
  | T_unsigned_int  -> pp_print_string fmt "unsigned int"
  | T_signed_long  -> pp_print_string fmt "signed long"
  | T_unsigned_long  -> pp_print_string fmt "unsigned long"
  | T_signed_long_long  -> pp_print_string fmt "signed long long"
  | T_unsigned_long_long -> pp_print_string fmt "unsigned long long"
  | T_signed_int128 -> pp_print_string fmt "signed int128"
  | T_unsigned_int128 -> pp_print_string fmt "unsigned int128"
  | T_float -> pp_print_string fmt "float"
  | T_double  -> pp_print_string fmt "double"
  | T_long_double  -> pp_print_string fmt "long double"
  | T_array(t, A_no_length) -> fprintf fmt "%a[]" pp_c_qual_typ t
  | T_array(t, A_constant_length len) -> fprintf fmt "%a[%a]" pp_c_qual_typ t Z.pp_print len
  | T_struct(s) -> fprintf fmt "struct %a" pp_var s
  | T_union(u) -> fprintf fmt "union %a" pp_var u
  | T_typedef(t) -> pp_var fmt t
  | T_pointer(t) -> fprintf fmt "%a *" pp_c_qual_typ t
  | T_enum(e) -> fprintf fmt "enum %a" pp_var e
  | T_unknown -> fprintf fmt "?"


let rec pp_formula fmt (f:formula with_range) =
  match get_content f with
  | F_expr e -> pp_expr fmt e
  | F_bool true  -> pp_print_string fmt "true"
  | F_bool false -> pp_print_string fmt "false"
  | F_binop (op, f1, f2) -> fprintf fmt "(%a)@\n%a (%a)" pp_formula f1 pp_log_binop op pp_formula f2
  | F_not f -> fprintf fmt "not (%a)" pp_formula f
  | F_forall (x, t, set, f) -> fprintf fmt "∀ %a %a ∈ %a: @[%a@]" pp_c_qual_typ t pp_var x pp_set set pp_formula f
  | F_exists (x, t, set, f) -> fprintf fmt "∃ %a %a ∈ %a: @[%a@]" pp_c_qual_typ t pp_var x pp_set set pp_formula f
  | F_predicate (p, params) -> fprintf fmt "%a(%a)" pp_var p (pp_list pp_expr "@., ") params
  | F_in (x, set) -> fprintf fmt "%a ∈ %a" pp_expr x pp_set set

and pp_log_binop fmt =
  function
  | AND -> pp_print_string fmt "and"
  | OR -> pp_print_string fmt "or"
  | IMPLIES -> pp_print_string fmt "implies"

and pp_set fmt =
  function
  | S_interval(e1, e2) -> fprintf fmt "[%a .. %a]" pp_expr e1 pp_expr e2
  | S_resource(r) -> pp_resource fmt r

let pp_opt pp fmt o =
  match o with
  | None -> ()
  | Some x -> pp fmt x

let rec pp_local fmt local =
  let local = get_content local in
  fprintf fmt "local    : %a %a = @[%a@];"
    pp_c_qual_typ local.ltyp
    pp_var local.lvar
    pp_local_value local.lval

and pp_local_value fmt v =
  match v with
  | L_new resource -> fprintf fmt "new %a" pp_resource resource
  | L_call (f, args) -> fprintf fmt "%a(%a)" pp_var f.content (pp_list pp_expr ", ") args


let pp_predicate fmt (predicate:predicate with_range) =
  fprintf fmt "predicate %a(%a): @[%a@];"
    pp_var predicate.content.predicate_var
    (pp_list pp_var ", ") predicate.content.predicate_args
    pp_formula predicate.content.predicate_body

let pp_requires fmt requires =
  fprintf fmt "requires : @[%a@];" pp_formula requires.content

let pp_assigns fmt assigns =
  fprintf fmt "assigns  : %a%a;"
    pp_expr assigns.content.assign_target
    (pp_print_list ~pp_sep:(fun fmt () -> ())
       (fun fmt (l, u) ->
          fprintf fmt "[%a .. %a]" pp_expr l pp_expr u
       )
    ) assigns.content.assign_offset

let pp_assumes fmt (assumes:assumes with_range) =
  fprintf fmt "assumes  : @[%a@];" pp_formula assumes.content

let pp_ensures fmt ensures =
  fprintf fmt "ensures  : @[%a@];" pp_formula ensures.content

let pp_free fmt free =
  fprintf fmt "free : %a;" pp_expr free.content

let pp_warn fmt warn =
  fprintf fmt "warn: \"%s\";" warn.content

let pp_leaf fmt sec =
  match sec with
  | S_local local -> pp_local fmt local
  | S_assumes assumes -> pp_assumes fmt assumes
  | S_requires requires -> pp_requires fmt requires
  | S_assigns assigns -> pp_assigns fmt assigns
  | S_ensures ensures -> pp_ensures fmt ensures
  | S_free free -> pp_free fmt free
  | S_warn warn  -> pp_warn fmt warn

let pp_case fmt case =
  fprintf fmt "case \"%s\":@\n  @[<v 2>%a@]"
    case.case_label
    (pp_list pp_leaf "@\n") case.case_body

let pp_alias fmt alias =
  fprintf fmt "alias: %s;" alias.content

let pp_section fmt sec =
  match sec with
  | S_leaf leaf -> pp_leaf fmt leaf
  | S_case case -> pp_case fmt case.content
  | S_predicate pred -> pp_predicate fmt pred
  | S_alias alias -> pp_alias fmt alias

let pp_sections fmt secs =
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
    pp_section
    fmt secs

let pp_stub fmt stub = pp_sections fmt stub.content 
