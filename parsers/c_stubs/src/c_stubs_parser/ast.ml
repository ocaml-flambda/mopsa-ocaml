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

(**
   Abstract syntax tree for C stubs. It is similar to the CST except:
   - predicates are expanded.
   - types and variables are resolved using the context of the Clang parser.
   - calls to sizeof are resolved using Clang's target information.
   - stubs and cases record their locals and assignments.
*)

open Location

type stub = {
  stub_name    : string;
  stub_params  : var list;
  stub_locals  : local with_range list;
  stub_assigns : assigns with_range list;
  stub_body    : section list;
  stub_alias   : string option;
  stub_range   : range;
}

(** {2 Stub sections} *)
(** ***************** *)

and section =
  | S_case      of case
  | S_leaf      of leaf

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
  case_locals    : local with_range list;
  case_assigns   : assigns with_range list;
  case_range     : range;
}

(** {2 Leaf sections} *)
(** ********************** *)

and requires = formula with_range

and ensures = formula with_range

and assumes = formula with_range

and local = {
  lvar : var;
  lval : local_value;
}

and local_value =
  | L_new of resource
  | L_call of C_AST.func with_range (** function *) * expr with_range list (* arguments *)

and assigns = {
  assign_target : expr with_range;
  assign_offset : (expr with_range * expr with_range) list;
}

and free = expr with_range

and warn = string

(** {2 Expressions} *)
(** *=*=*=*=*=*=*=* *)

and expr_kind =
  | E_top       of C_AST.type_qual
  | E_int       of Z.t
  | E_float     of float
  | E_string    of string
  | E_char      of int
  | E_invalid

  | E_var       of var

  | E_unop      of unop  * expr with_range
  | E_binop     of binop * expr with_range * expr with_range

  | E_addr_of   of expr with_range
  | E_deref     of expr with_range
  | E_cast      of C_AST.type_qual * bool (** is it explicit? *) * expr with_range

  | E_subscript of expr with_range * expr with_range
  | E_member    of expr with_range * string
  | E_attribute of expr with_range * string
  | E_arrow     of expr with_range * string

  | E_builtin_call  of builtin * expr with_range

  | E_return

and expr = {
  kind : expr_kind;
  typ  : C_AST.type_qual;
}

and log_binop = Cst.log_binop

and binop = Cst.binop

and unop = Cst.unop

and set =
  | S_interval of expr with_range * expr with_range
  | S_resource of resource

and resource = Cst.resource

and var = C_AST.variable

and builtin = Cst.builtin


(** {2 Formulas} *)
(** ************ *)

and formula =
  | F_expr   of expr with_range
  | F_bool   of bool
  | F_binop  of log_binop * formula with_range * formula with_range
  | F_not    of formula with_range
  | F_forall of var * set * formula with_range
  | F_exists of var * set * formula with_range
  | F_in     of expr with_range * set


(** {2 Utility functions} *)
(** ********************* *)


let compare_var v1 v2 =
  Compare.compose [
    (fun () -> compare v1.C_AST.var_unique_name v2.C_AST.var_unique_name);
    (fun () -> compare v1.C_AST.var_uid v2.C_AST.var_uid);
  ]

let compare_resource r1 r2 = Cst.compare_resource r1 r2

(** {2 Pretty printers} *)
(** ******************* *)

open Format

let pp_var fmt v = pp_print_string fmt v.C_AST.var_org_name

let pp_resource = Cst.pp_resource

let pp_builtin = Cst.pp_builtin

let rec pp_expr fmt exp =
  match exp.content.kind with
  | E_top(t) -> fprintf fmt "⊤(%a)" pp_c_qual_typ t
  | E_int n -> Z.pp_print fmt n
  | E_float f -> pp_print_float fmt f
  | E_string s -> fprintf fmt "\"%s\"" s
  | E_char c ->
    if c >= 32 && c < 127
    then fprintf fmt "'%c'" (Char.chr c)
    else fprintf fmt "%d" c
  | E_invalid -> fprintf fmt "INVALID"
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
  | E_attribute(s, f) -> fprintf fmt "%a:%s" pp_expr s f
  | E_arrow(p, f) -> fprintf fmt "%a->%s" pp_expr p f
  | E_builtin_call(f, arg) -> fprintf fmt "%a(%a)" pp_builtin f pp_expr arg
  | E_return -> pp_print_string fmt "return"

and pp_unop = Cst.pp_unop

and pp_binop = Cst.pp_binop

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
  | F_in (x, set) -> fprintf fmt "%a ∈ %a" pp_expr x pp_set set

and pp_log_binop = Cst.pp_log_binop

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
  let local = get_content local in
  fprintf fmt "local    : %a %a = @[%a@];"
    pp_c_qual_typ local.lvar.var_type
    pp_var local.lvar
    pp_local_value local.lval

and pp_local_value fmt v =
  match v with
  | L_new resource -> fprintf fmt "new %a" pp_resource resource
  | L_call (f, args) -> fprintf fmt "%s(%a)" f.content.func_org_name (pp_list pp_expr ", ") args

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

let pp_leaf_section fmt sec =
  match sec with
  | S_local local -> pp_local fmt local
  | S_assumes assumes -> pp_assumes fmt assumes
  | S_requires requires -> pp_requires fmt requires
  | S_assigns assigns -> pp_assigns fmt assigns
  | S_ensures ensures -> pp_ensures fmt ensures
  | S_free free -> pp_free fmt free
  | S_warn warn  -> pp_warn fmt warn

let pp_leaf_sections fmt secs =
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
    pp_leaf_section
    fmt secs

let pp_case fmt case =
  fprintf fmt "case \"%s\":@\n  @[<v 2>%a@]"
    case.case_label
    pp_leaf_sections case.case_body

let pp_section fmt sec =
  match sec with
  | S_leaf leaf -> pp_leaf_section fmt leaf
  | S_case case -> pp_case fmt case

let pp_sections fmt secs =
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
    pp_section
    fmt secs

let pp_stub fmt stub =
  match stub.stub_alias with
  | None -> pp_sections fmt stub.stub_body
  | Some alias -> fprintf fmt "alias: %s;" alias
