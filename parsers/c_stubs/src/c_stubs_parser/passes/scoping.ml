(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Resolve variables scope and give unique identifiers accordingly. *)

open Location
open Cst


(* {2 Scope definition} *)
(* ******************** *)

module Scope =
struct

  include SetExt.Make(struct type t = var let compare = compare_var end)

  let uid_counter = ref 0

  let create v s =
    incr uid_counter;
    let v' = { v with vuid = !uid_counter } in
    let s' = filter (fun v' -> v'.vname != v.vname ) s |>
             add v
    in
    v', s'

  let resolve v s =
    filter (fun v' -> v'.vname = v.vname) s |>
    elements |>
    function
    | [v] -> v
    | [] -> v
    | _ -> Exceptions.panic "Too many variables %a in scope" pp_var v

end

(* {2 Visitors} *)
(* ************ *)

let rec visit_list f l scope =
  match l with
  | [] -> [], scope
  | hd :: tl ->
    let hd', scope' = f hd scope in
    let tl', scope'' = visit_list f tl scope' in
    hd' :: tl', scope''

let visit_option f o scope =
  match o with
  | None -> None, scope
  | Some x ->
    let x, scope = f x in
    Some x, scope

let rec visit_expr (expr:expr with_range) scope =
  bind_pair_range expr @@ fun expr ->
  match expr with
  | E_int _ | E_float _
  | E_string _ | E_char _ | E_return ->
    expr, scope

  | E_var v ->
    E_var (Scope.resolve v scope), scope

  | E_unop(op, e) ->
    let e, scope = visit_expr e scope in
    E_unop(op, e), scope

  | E_binop (op, e1, e2) ->
    let e1, scope = visit_expr e1 scope in
    let e2, scope = visit_expr e2 scope in
    E_binop (op, e1, e2), scope

  | E_addr_of e ->
    let e, scope = visit_expr e scope in
    E_addr_of e, scope

  | E_deref e ->
    let e, scope = visit_expr e scope in
    E_deref e, scope

  | E_cast (t, e) ->
    let e, scope = visit_expr e scope in
    E_cast (t, e), scope

  | E_subscript (a, i) ->
    let a, scope = visit_expr a scope in
    let i, scope = visit_expr i scope in
    E_subscript (a, i), scope

  | E_member (s, f) ->
    let s, scope = visit_expr s scope in
    E_member (s, f), scope

  | E_arrow (p, f) ->
    let p, scope = visit_expr p scope in
    E_arrow (p, f), scope

  | E_builtin_call (f, arg) ->
    let arg, scope = visit_expr arg scope in
    E_builtin_call (f, arg), scope

let rec visit_formula (formula:formula with_range) scope =
  bind_pair_range formula @@ fun formula ->
  match formula with
  | F_expr e ->
    let e, scope = visit_expr e scope in
    F_expr e, scope

  | F_bool _ -> formula, scope

  | F_binop (op, f1, f2) ->
    let f1, scope = visit_formula f1 scope in
    let f2, scope = visit_formula f2 scope in
    F_binop (op, f1, f2), scope

  | F_not f ->
    let f, scope = visit_formula f scope in
    F_not f, scope

  | F_forall (v, t, s, f) ->
    let v, scope' = Scope.create v scope in
    let f, scope' = visit_formula f scope' in
    F_forall (v, t, s, f), scope

  | F_exists (v, t, s, f) ->
    let v, scope' = Scope.create v scope in
    let f, scope' = visit_formula f scope' in
    F_exists (v, t, s, f), scope

  | F_predicate(p, params) ->
    let p = Scope.resolve p scope in
    let params, scope = visit_list visit_expr params scope in
    F_predicate(p, params), scope

  | F_in (v, s) ->
    let v = Scope.resolve v scope in
    F_in (v, s), scope

  | F_free e ->
    let e, scope = visit_expr e scope in
    F_free e, scope

let visit_predicate pred scope =
  bind_pair_range pred @@ fun pred ->
  let v, scope = Scope.create pred.predicate_var scope in
  let args, scope = List.fold_left (fun (args, scope) arg ->
      let arg, scope = Scope.create arg scope in arg :: args, scope
    ) ([], scope) pred.predicate_args
  in
  let body, scope = visit_formula pred.predicate_body scope in
  { predicate_var = v; predicate_args = List.rev args; predicate_body = body}, scope

let visit_requires requires scope =
  bind_pair_range requires @@ fun requires ->
  visit_formula requires scope

let visit_assumes assumes scope =
  bind_pair_range assumes @@ fun assumes ->
  visit_formula assumes scope

let visit_ensures ensures scope =
  bind_pair_range ensures @@ fun ensures ->
  visit_formula ensures scope

let visit_assigns assigns scope =
  bind_pair_range assigns @@ fun assigns ->
  let assigns_target, scope = visit_expr assigns.assigns_target scope in
  let assigns_range, scope = visit_option (fun (l, u) ->
      let l, scope = visit_expr l scope in
      let u, scope = visit_expr u scope in
      (l, u), scope
    ) assigns.assigns_range scope
  in
  { assigns_target; assigns_range }, scope

let visit_local_value lv scope =
  match lv with
  | Local_new rc -> lv, scope
  | Local_function_call (f, args) ->
    let f, scope = visit_expr f scope in
    let args, scope = visit_list visit_expr args scope in
    Local_function_call (f, args), scope

let visit_local local scope =
  bind_pair_range local @@ fun local ->
  let local_value, scope = visit_local_value local.local_value scope in
  let local_var, scope = Scope.create local.local_var scope in
  { local_var; local_typ = local.local_typ; local_value }, scope

let visit_case c scope =
  bind_pair_range c @@ fun c ->
  let requires, scope = visit_list visit_requires c.case_requires scope in
  let assumes, scope = visit_list visit_assumes c.case_assumes scope in
  let assigns, scope = visit_list visit_assigns c.case_assigns scope in
  let local, scope' = visit_list visit_local c.case_local scope in
  let ensures, scope' = visit_list visit_ensures c.case_ensures scope' in

  {
    case_label = c.case_label;
    case_assumes = assumes;
    case_requires  = requires;
    case_assigns = assigns;
    case_local = local;
    case_ensures = ensures;
  }, scope


(** {2 Entry point} *)
(** *************** *)

let doit (stub: stub with_range) : stub with_range =
  bind_range stub @@ fun stub ->
  match stub with
  | S_simple s ->
    let predicates, scope = visit_list visit_predicate s.simple_stub_predicates Scope.empty in
    let requires, scope = visit_list visit_requires s.simple_stub_requires scope in
    let assigns, scope = visit_list visit_assigns s.simple_stub_assigns scope in
    let local, scope = visit_list visit_local s.simple_stub_local scope in
    let ensures, scope = visit_list visit_ensures s.simple_stub_ensures scope in

    S_simple {
      simple_stub_predicates = predicates;
      simple_stub_requires  = requires;
      simple_stub_assigns = assigns;
      simple_stub_local = local;
      simple_stub_ensures = ensures;
    }

  | S_case s ->
    let predicates, scope = visit_list visit_predicate s.case_stub_predicates Scope.empty in
    let requires, scope = visit_list visit_requires s.case_stub_requires scope in
    let cases, scope = visit_list visit_case s.case_stub_cases scope in

    S_case {
      case_stub_predicates = predicates;
      case_stub_requires  = requires;
      case_stub_cases = cases;
    }
