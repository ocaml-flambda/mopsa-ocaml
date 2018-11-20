(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Concrete syntax trees for C stubs *)

open Location

type stub =
  | S_simple of simple_stub
  | S_case of case_stub

and simple_stub = {
  simple_stub_predicates : predicate with_range list;
  simple_stub_requires   : requires with_range list;
  simple_stub_assigns    : assigns with_range list;
  simple_stub_local      : local with_range list;
  simple_stub_ensures    : ensures with_range list;
}

and case_stub = {
  case_stub_predicates : predicate with_range list;
  case_stub_requires   : requires with_range list;
  case_stub_cases      : case with_range list;
}

and requires = formula with_range
and ensures = formula with_range
and assumes = formula with_range

and local = {
  local_var   : var;
  local_typ   : c_qual_typ;
  local_value : local_value;
}

and local_value =
  | Local_new           of resource
  | Local_function_call of expr with_range (** function *) * expr with_range list (* arguments *)

and assigns = {
    assigns_target : expr with_range;
    assigns_range  : (expr with_range * expr with_range) option;
  }

and case = {
    case_label: string;
    case_assumes: assumes with_range list;
    case_requires : requires with_range list;
    case_assigns  : assigns with_range list;
    case_local    : local with_range list;
    case_ensures  : ensures with_range list;
  }

and formula =
  | F_expr   of expr with_range
  | F_bool   of bool
  | F_binop  of log_binop * formula with_range * formula with_range
  | F_not    of formula with_range
  | F_forall of var * c_qual_typ * set * formula with_range
  | F_exists of var * c_qual_typ * set * formula with_range
  | F_in     of var * set
  | F_free   of expr with_range

and expr =
  | E_int       of Z.t
  | E_float     of float
  | E_string    of string
  | E_char      of char

  | E_var       of var

  | E_unop      of unop  * expr with_range
  | E_binop     of binop * expr with_range * expr with_range

  | E_addr_of   of expr with_range
  | E_deref     of expr with_range
  | E_cast      of c_qual_typ * expr with_range

  | E_subscript of expr with_range * expr with_range
  | E_member    of expr with_range * string
  | E_arrow     of expr with_range * string

  | E_builtin_call  of builtin * expr with_range

  | E_return

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
  | T_array of c_qual_typ * expr with_range option
  | T_struct of var
  | T_union of var
  | T_typedef of var
  | T_pointer of c_qual_typ
  | T_enum of var


and predicate = {
  predicate_var  : var;
  predicate_args : var list;
  predicate_body : formula with_range;
}

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
  vname: string;
  vuid : int;
}

and builtin =
  | OLD
  | SIZE
  | OFFSET
  | BASE


(** {2 Comparison functions} *)
(** ************************ *)

let compare_var v1 v2 =
  Compare.compose [
    (fun () -> compare v1.vname v2.vname);
    (fun () -> compare v1.vuid v2.vuid);
  ]

(** {2 Pretty printer} *)
(** ****************** *)

open Format

let pp_var fmt v = pp_print_string fmt v.vname

let pp_resource fmt resource = pp_print_string fmt resource

let pp_builtin fmt f =
  match f with
  | SIZE   -> pp_print_string fmt "size"
  | OFFSET -> pp_print_string fmt "offset"
  | BASE   -> pp_print_string fmt "base"
  | OLD    -> pp_print_string fmt "old"


let rec pp_expr fmt exp =
  match get_range_content exp with
  | E_int n -> Z.pp_print fmt n
  | E_float f -> pp_print_float fmt f
  | E_string s -> fprintf fmt "\"%s\"" s
  | E_char c -> fprintf fmt "'%c'" c
  | E_var v -> pp_var fmt v
  | E_unop (op, e) -> fprintf fmt "%a(%a)" pp_unop op pp_expr e
  | E_binop (op, e1, e2) -> fprintf fmt "(%a) %a (%a)" pp_expr e1 pp_binop op pp_expr e2
  | E_addr_of e -> fprintf fmt "&(%a)" pp_expr e
  | E_deref e -> fprintf fmt "*(%a)" pp_expr e
  | E_cast(t, e) -> fprintf fmt "(%a) %a" pp_c_qual_typ t pp_expr e
  | E_subscript(a, i) -> fprintf fmt "%a[%a]" pp_expr a pp_expr i
  | E_member(s, f) -> fprintf fmt "%a.%s" pp_expr s f
  | E_arrow(p, f) -> fprintf fmt "%a->%s" pp_expr p f
  | E_builtin_call(f, arg) -> fprintf fmt "%a(%a)" pp_builtin f pp_expr arg
  | E_return -> pp_print_string fmt "return"

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
  | T_array(t, None) -> fprintf fmt "%a[]" pp_c_qual_typ t
  | T_array(t, Some len) -> fprintf fmt "%a[%a]" pp_c_qual_typ t pp_expr len
  | T_struct(s) -> fprintf fmt "struct %a" pp_var s
  | T_union(u) -> fprintf fmt "union %a" pp_var u
  | T_typedef(t) -> pp_var fmt t
  | T_pointer(t) -> fprintf fmt "%a *" pp_c_qual_typ t
  | T_enum(e) -> fprintf fmt "enum %a" pp_var e


let rec pp_formula fmt (f:formula with_range) =
  match get_range_content f with
  | F_expr e -> pp_expr fmt e
  | F_bool true  -> pp_print_string fmt "true"
  | F_bool false -> pp_print_string fmt "false"
  | F_binop (op, f1, f2) -> fprintf fmt "(%a) %a (%a)" pp_formula f1 pp_log_binop op pp_formula f2
  | F_not f -> fprintf fmt "not (%a)" pp_formula f
  | F_forall (x, t, set, f) -> fprintf fmt "∀ %a %a ∈ %a: @[%a@]" pp_c_qual_typ t pp_var x pp_set set pp_formula f
  | F_exists (x, t, set, f) -> fprintf fmt "∃ %a %a ∈ %a: @[%a@]" pp_c_qual_typ t pp_var x pp_set set pp_formula f
  | F_in (x, set) -> fprintf fmt "%a ∈ %a" pp_var x pp_set set
  | F_free e -> fprintf fmt "free(%a)" pp_expr e

and pp_log_binop fmt =
  function
  | AND -> pp_print_string fmt "and"
  | OR -> pp_print_string fmt "or"
  | IMPLIES -> pp_print_string fmt "implies"

and pp_set fmt =
  function
  | S_interval(e1, e2) -> fprintf fmt "[%a .. %a]" pp_expr e1 pp_expr e2
  | S_resource(r) -> pp_resource fmt r

let pp_list pp sep fmt l =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt sep) pp fmt l

let pp_opt pp fmt o =
  match o with
  | None -> ()
  | Some x -> pp fmt x


let rec pp_local fmt local =
  let local = get_range_content local in
  fprintf fmt "local: %a %a = @[%a@];"
    pp_c_qual_typ local.local_typ
    pp_var local.local_var
    pp_local_value local.local_value

and pp_local_value fmt v =
  match v with
  | Local_new resouce -> fprintf fmt "new %a" pp_resource resouce
  | Local_function_call (f, args) -> fprintf fmt "%a(%a)" pp_expr f (pp_list pp_expr ", ") args


let pp_predicate fmt (predicate:predicate with_range) =
  fprintf fmt "predicate %a(%a): @[%a@];"
    pp_var predicate.content.predicate_var
    (pp_list pp_var ", ") predicate.content.predicate_args
    pp_formula predicate.content.predicate_body

let pp_requires fmt requires =
  fprintf fmt "requires: @[%a@];" pp_formula requires.content

let pp_assigns fmt assigns =
  fprintf fmt "assigns: %a%a;"
    pp_expr assigns.content.assigns_target
    (pp_opt (fun fmt (l, u) ->
         fprintf fmt "[%a .. %a]" pp_expr l pp_expr u
       )
    ) assigns.content.assigns_range


let pp_assumes fmt (assumes:assumes with_range) =
  fprintf fmt "assumes: @[%a@];" pp_formula assumes.content

let pp_ensures fmt ensures =
  fprintf fmt "ensures: @[%a@];" pp_formula ensures.content

let pp_case fmt case =
  fprintf fmt "case \"%s\":@\n  @[%a%a%a%a%a@]"
    case.content.case_label
    (pp_list pp_assumes "@\n") case.content.case_assumes
    (pp_list pp_local "@\n") case.content.case_local
    (pp_list pp_requires "@\n") case.content.case_requires
    (pp_list pp_assigns "@\n") case.content.case_assigns
    (pp_list pp_ensures "@\n") case.content.case_ensures

let pp_stub fmt stub =
  match stub with
  | S_simple ss ->
    fprintf fmt "%a%a%a%a%a"
      (pp_list pp_predicate "@\n") ss.simple_stub_predicates
      (pp_list pp_requires "@\n") ss.simple_stub_requires
      (pp_list pp_assigns "@\n") ss.simple_stub_assigns
      (pp_list pp_local "@\n") ss.simple_stub_local
      (pp_list pp_ensures "@\n") ss.simple_stub_ensures

  | S_case ms ->
    fprintf fmt "%a%a%a"
      (pp_list pp_predicate "@\n") ms.case_stub_predicates
      (pp_list pp_requires "@\n") ms.case_stub_requires
      (pp_list pp_case "@\n") ms.case_stub_cases
