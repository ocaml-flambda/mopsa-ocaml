(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstract syntax trees for C stubs *)

open Location


(** {2 Expressions} *)
(** *=*=*=*=*=*=*=* *)

type expr_kind =
  | E_int       of Z.t
  | E_float     of float
  | E_string    of string
  | E_char      of char

  | E_var       of var

  | E_unop      of unop  * expr with_range
  | E_binop     of binop * expr with_range * expr with_range

  | E_addr_of   of expr with_range
  | E_deref     of expr with_range
  | E_cast      of C_AST.type_qual * bool (** is it explicit? *) * expr with_range

  | E_subscript of expr with_range * expr with_range
  | E_member    of expr with_range * string
  | E_arrow     of expr with_range * string

  | E_builtin_call  of builtin * expr with_range

  | E_return

and expr = {
  kind : expr_kind;
  typ  : C_AST.type_qual;
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

and var = C_AST.variable

and builtin =
  | PRIMED
  | SIZE
  | OFFSET
  | BASE
  | FLOAT_VALID
  | FLOAT_INF
  | FLOAT_NAN


(** {2 Formulas} *)
(** ************ *)

type formula =
  | F_expr   of expr with_range
  | F_bool   of bool
  | F_binop  of log_binop * formula with_range * formula with_range
  | F_not    of formula with_range
  | F_forall of var * set * formula with_range
  | F_exists of var * set * formula with_range
  | F_in     of var * set
  | F_free   of expr with_range



(** {2 Pre-condition} *)
(** ***************** *)

type requires = formula with_range
type assumes  = formula with_range



(** {2 Post-condition} *)
(** ****************** *)

type ensures = formula with_range

type local = {
    lvar : var;
    lval : local_value;
  }

and local_value =
  | L_new  of resource
  | L_call of C_AST.func with_range (** function *) * expr with_range list (* arguments *)

type assigns = {
  assign_target: expr with_range;
  assign_offset: (expr with_range * expr with_range) option;  (** offset *)
}


(** {2 Stubs} *)
(** ********* *)

type stub = {
  stub_requires: requires with_range list;
  stub_body: body;
}

and body =
  | B_post  of post
  | B_cases of case with_range list

and post = {
  post_assigns  : assigns with_range list;
  post_local    : local with_range list;
  post_ensures  : ensures with_range list;
}

and case = {
  case_label    : string;
  case_assumes  : assumes with_range list;
  case_requires : requires with_range list;
  case_post : post;
}


let compare_var v1 v2 =
  Compare.compose [
    (fun () -> compare v1.C_AST.var_unique_name v2.C_AST.var_unique_name);
    (fun () -> compare v1.C_AST.var_uid v2.C_AST.var_uid);
  ]


(** Pretty printer *)
(** ============== *)

open Format

let pp_var fmt v = pp_print_string fmt v.C_AST.var_org_name

let pp_resource fmt resource = pp_print_string fmt resource

let pp_builtin fmt f =
  match f with
  | PRIMED -> pp_print_string fmt "primed"
  | SIZE   -> pp_print_string fmt "size"
  | OFFSET -> pp_print_string fmt "offset"
  | BASE   -> pp_print_string fmt "base"
  | FLOAT_VALID -> pp_print_string fmt "float_valid"
  | FLOAT_INF   -> pp_print_string fmt "float_inf"
  | FLOAT_NAN   -> pp_print_string fmt "float_nan"


let rec pp_expr fmt exp =
  match exp.content.kind with
  | E_int n -> Z.pp_print fmt n
  | E_float f -> pp_print_float fmt f
  | E_string s -> fprintf fmt "\"%s\"" s
  | E_char c -> fprintf fmt "'%c'" c
  | E_var v -> pp_var fmt v
  | E_unop (op, e) -> fprintf fmt "%a(%a)" pp_unop op pp_expr e
  | E_binop (op, e1, e2) -> fprintf fmt "(%a) %a (%a)" pp_expr e1 pp_binop op pp_expr e2
  | E_addr_of e -> fprintf fmt "&(%a)" pp_expr e
  | E_deref e -> fprintf fmt "*(%a)" pp_expr e
  | E_cast(t, explicit, e) ->
    if explicit then fprintf fmt "(%a) %a" pp_c_qual_typ t pp_expr e
    else pp_expr fmt e
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

and pp_c_qual_typ fmt t = Format.pp_print_string fmt (C_print.string_of_type_qual t)


let rec pp_formula fmt (f:formula with_range) =
  match f.content with
  | F_expr e -> pp_expr fmt e
  | F_bool true  -> pp_print_string fmt "true"
  | F_bool false -> pp_print_string fmt "false"
  | F_binop (op, f1, f2) -> fprintf fmt "(%a) %a (%a)" pp_formula f1 pp_log_binop op pp_formula f2
  | F_not f -> fprintf fmt "not (%a)" pp_formula f
  | F_forall (x, set, f) -> fprintf fmt "∀ %a %a ∈ %a: @[%a@]" pp_c_qual_typ x.C_AST.var_type pp_var x pp_set set pp_formula f
  | F_exists (x, set, f) -> fprintf fmt "∃ %a %a ∈ %a: @[%a@]" pp_c_qual_typ x.C_AST.var_type pp_var x pp_set set pp_formula f
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
  fprintf fmt "local: %a %a = @[%a@];"
    pp_c_qual_typ local.content.lvar.C_AST.var_type
    pp_var local.content.lvar
    pp_local_value local.content.lval

and pp_local_value fmt v =
  match v with
  | L_new resouce -> fprintf fmt "new %a" pp_resource resouce
  | L_call (f, args) -> fprintf fmt "%s(%a)" f.content.func_org_name (pp_list pp_expr ", ") args

let pp_requires fmt requires =
  fprintf fmt "requires: @[%a@];" pp_formula requires.content

let pp_assigns fmt assigns =
  fprintf fmt "assigns: %a%a;"
    pp_expr assigns.content.assign_target
    (pp_opt (fun fmt (l, u) ->
         fprintf fmt "[%a .. %a]" pp_expr l pp_expr u
       )
    ) assigns.content.assign_offset

let pp_assumes fmt (assumes:assumes with_range) =
  fprintf fmt "assumes: @[%a@];" pp_formula assumes.content

let pp_ensures fmt ensures =
  fprintf fmt "ensures: @[%a@];" pp_formula ensures.content

let pp_post fmt post =
  fprintf fmt "%a%a%a"
    (pp_list pp_assigns "@\n") post.post_assigns
    (pp_list pp_local "@\n") post.post_local
    (pp_list pp_ensures "@\n") post.post_ensures

let pp_case fmt case =
  fprintf fmt "case \"%s\":@\n  @[%a%a%a@]"
    case.content.case_label
    (pp_list pp_assumes "@\n") case.content.case_assumes
    (pp_list pp_requires "@\n") case.content.case_requires
    pp_post case.content.case_post

let pp_body fmt body =
  match body with
  | B_post post -> pp_post fmt post
  | B_cases cases  -> (pp_list pp_case "@\n") fmt cases

let pp_stub fmt stub =
  fprintf fmt "%a%a"
    (pp_list pp_requires "@\n") stub.stub_requires
    pp_body stub.stub_body
