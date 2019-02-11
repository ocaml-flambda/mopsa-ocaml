(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(* Inline macro definitions *)

open Cst
open Location
module M = MapExt.StringMap

let debug fmt = Debug.debug ~channel:"c_stubs.passes.macro_expansion" fmt

let rec visit_expr macros enums expr =
  bind_range expr @@ fun e ->
  match e with
  | E_int _ | E_float _ | E_invalid
  | E_string _ | E_char _ | E_return ->
    e

  | E_var v when M.mem v.vname enums ->
    let value = M.find v.vname enums in
    E_int (value, NO_SUFFIX)

  | E_var v when M.mem v.vname macros ->
    let lexeme = M.find v.vname macros in
    let lexbuf = Lexing.from_string lexeme in
    let e' = Parser.parse_expr Lexer.read lexbuf in
    get_content @@ visit_expr macros enums (with_range e' expr.range)

  | E_var v ->
    E_var v

  | E_unop(op, e) ->
    let e = visit_expr macros enums e in
    E_unop(op, e)

  | E_binop (op, e1, e2) ->
    let e1 = visit_expr macros enums e1 in
    let e2 = visit_expr macros enums e2 in
    E_binop (op, e1, e2)

  | E_addr_of e ->
    let e = visit_expr macros enums e  in
    E_addr_of e

  | E_deref e ->
    let e = visit_expr macros enums e in
    E_deref e

  | E_cast (t, e) ->
    let e = visit_expr macros enums e in
    E_cast (t, e)

  | E_subscript (a, i) ->
    let a = visit_expr macros enums a in
    let i = visit_expr macros enums i in
    E_subscript (a, i)

  | E_member (s, f) ->
    let s = visit_expr macros enums s  in
    E_member (s, f)

  | E_attribute (o, f) ->
    let o = visit_expr macros enums o in
    E_attribute (o, f)

  | E_arrow (p, f) ->
    let p = visit_expr macros enums p in
    E_arrow (p, f)

  | E_builtin_call (f, arg) ->
    let arg = visit_expr macros enums arg in
    E_builtin_call (f, arg)

  | E_sizeof_var v when M.mem v.content.vname macros ->
    let lexeme = M.find v.content.vname macros in
    let lexbuf = Lexing.from_string lexeme in
    let e' = Parser.parse_expr Lexer.read lexbuf in
    let e' = visit_expr macros enums (with_range e' v.range) in
    E_sizeof_expr e'

  | E_sizeof_var v ->
    E_sizeof_var v

  | E_sizeof_type t ->
    E_sizeof_type t

  | E_sizeof_expr e ->
    let e = visit_expr macros enums e in
    E_sizeof_expr e

let visit_set macros enums set =
  match set with
  | S_interval(e1, e2) ->
    let e1 = visit_expr macros enums e1 in
    let e2 = visit_expr macros enums e2 in
    S_interval(e1, e2)

  | S_resource r -> S_resource r

let rec visit_formula macros enums formula =
  bind_range formula @@ fun f ->
  match f with
  | F_expr e ->
    let e = visit_expr macros enums e in
    F_expr e

  | F_bool _ -> f

  | F_binop (op, f1, f2) ->
    let f1 = visit_formula macros enums f1 in
    let f2 = visit_formula macros enums f2 in
    F_binop (op, f1, f2)

  | F_not f ->
    let f = visit_formula macros enums f in
    F_not f

  | F_forall (v, t, s, f) ->
    let f = visit_formula macros enums f in
    let s = visit_set macros enums s in
    F_forall (v, t, s, f)

  | F_exists (v, t, s, f) ->
    let f = visit_formula macros enums f in
    let s = visit_set macros enums s in
    F_exists (v, t, s, f)

  | F_predicate(p, params) -> Exceptions.panic "macro_expansion: predicates not supported"

  | F_in (e, s) ->
    let e = visit_expr macros enums e in
    let s = visit_set macros enums s in
    F_in (e, s)

let visit_assumes macros enums assumes =
  bind_range assumes @@ visit_formula macros enums

let visit_requires macros enums requires =
  bind_range requires @@ visit_formula macros enums

let visit_ensures macros enums ensures =
  bind_range ensures @@ visit_formula macros enums

let visit_free macros enums free =
  bind_range free @@ visit_expr macros enums

let visit_local macros enums local =
  bind_range local @@ fun local -> {
    lvar = local.lvar;
    ltyp = local.ltyp;
    lval =
      match local.lval with
      | L_new r -> L_new r
      | L_call(f, args) -> L_call(f, List.map (visit_expr macros enums) args)
  }

let visit_assigns macros enums assigns =
  bind_range assigns @@ fun assigns -> {
    assign_target = visit_expr macros enums assigns.assign_target;
    assign_offset = List.map (fun (a, b) ->
        (visit_expr macros enums a),
        (visit_expr macros enums b)
      ) assigns.assign_offset;
  }

let visit_leaf macros enums leaf =
  match leaf with
  | S_local local       -> S_local (visit_local macros enums local)
  | S_assumes assumes   -> S_assumes (visit_assumes macros enums assumes)
  | S_requires requires -> S_requires (visit_requires macros enums requires)
  | S_assigns assigns   -> S_assigns (visit_assigns macros enums assigns)
  | S_ensures ensures   -> S_ensures (visit_ensures macros enums ensures)
  | S_free free         -> S_free (visit_free macros enums free)
  | S_warn warn         -> S_warn warn

let visit_case macros enums case =
  bind_range case @@ fun case -> {
    case_label = case.case_label;
    case_body = List.map (visit_leaf macros enums) case.case_body;
  }

let visit_section macros enums section =
  match section with
  | S_case case -> S_case (visit_case macros enums case)
  | S_leaf leaf -> S_leaf (visit_leaf macros enums leaf)
  | S_predicate _ -> Exceptions.panic "macro_expansion: predicates not supported"
  | S_alias _ -> section

let visit_sections_list macros enums sections =
  List.map (fun section -> visit_section macros enums section) sections

let doit (stub:stub) macros enums =
  bind_range stub @@ visit_sections_list macros enums
