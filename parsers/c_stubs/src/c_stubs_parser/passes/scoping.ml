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


let debug fmt = Debug.debug ~channel:"c_stubs_parser.passes.scoping" fmt


(* {2 Scope definition} *)
(* ******************** *)

module Scope =
struct

  include SetExt.Make(struct type t = var let compare v1 v2 = compare v1.vname v2.vname end)

  let uid_counter = ref 1000

  let create v t s =
    incr uid_counter;
    let v' = {
      vname = v.vname;
      vuid = !uid_counter;
      vlocal = true;
      vrange = v.vrange;
      vtyp = t;
    } in
    let s' = remove v' s |>
             add v'
    in
    v', s'

  let resolve v s =
    match find_opt v s with
    | None -> v
    | Some vv -> vv

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
    let x, scope = f x scope in
    Some x, scope

let rec visit_expr (e:expr with_range) scope =
  bind_pair_range e @@ fun expr ->
  match expr with
  | E_int _ | E_float _ | E_invalid
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

  | E_attribute (o, f) ->
    let o, scope = visit_expr o scope in
    E_attribute (o, f), scope

  | E_arrow (p, f) ->
    let p, scope = visit_expr p scope in
    E_arrow (p, f), scope

  | E_builtin_call (f, arg) ->
    let arg, scope = visit_expr arg scope in
    E_builtin_call (f, arg), scope

  | E_sizeof_var v ->
    (* If the variable is in the scope, then we are sure this a sizeof on a expression *)
    if Scope.mem v.content scope then
      let vv = bind_range v (fun v -> E_var (Scope.resolve v scope)) in
      E_sizeof_expr vv, scope
    else
      (* Otherwise, we wait for the Cst_to_ast pass to resolve it using project info *)
      E_sizeof_var v, scope

  | E_sizeof_type t ->
    E_sizeof_type t, scope

  | E_sizeof_expr e ->
    let e, scope = visit_expr e scope in
    E_sizeof_expr e, scope

let visit_set (set:set) scope =
  match set with
  | S_interval(e1, e2) ->
    let e1, scope = visit_expr e1 scope in
    let e2, scope = visit_expr e2 scope in
    S_interval(e1, e2), scope

  | S_resource r -> S_resource r, scope

let rec visit_formula (f:formula with_range) scope =
  bind_pair_range f @@ fun formula ->
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
    let v, scope' = Scope.create v t scope in
    let f, scope' = visit_formula f scope' in
    let s, _ = visit_set s scope in
    F_forall (v, t, s, f), scope

  | F_exists (v, t, s, f) ->
    let v, scope' = Scope.create v t scope in
    let f, scope' = visit_formula f scope' in
    let s, _ = visit_set s scope in
    F_exists (v, t, s, f), scope

  | F_predicate(p, params) ->
    let p = Scope.resolve p scope in
    let params, scope = visit_list visit_expr params scope in
    F_predicate(p, params), scope

  | F_in (e, s) ->
    let e, scope = visit_expr e scope in
    let s, scope = visit_set s scope in
    F_in (e, s), scope

let visit_predicate pred scope = Exceptions.panic "scoping: predicate not expanded"

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
  let assign_target, scope = visit_expr assigns.assign_target scope in
  let assign_offset, scope = visit_option (visit_list (fun (l, u) scope ->
      let l, scope = visit_expr l scope in
      let u, scope = visit_expr u scope in
      (l, u), scope
    )) assigns.assign_offset scope
  in
  { assign_target; assign_offset }, scope

let visit_free free scope =
  bind_pair_range free @@ fun free ->
  visit_expr free scope

let visit_local_value lv scope =
  match lv with
  | L_new rc -> lv, scope
  | L_call (f, args) ->
    let f = bind_range f @@ fun ff -> Scope.resolve ff scope in
    let args, scope = visit_list visit_expr args scope in
    L_call (f, args), scope

let visit_local local scope =
  bind_pair_range local @@ fun local ->
  let lval, scope = visit_local_value local.lval scope in
  let lvar, scope = Scope.create local.lvar local.ltyp scope in
  { lvar; ltyp = local.ltyp; lval }, scope

let visit_leaf leaf scope =
  match leaf with
  | S_local local ->
    let local, scope = visit_local local scope in
    S_local local, scope

  | S_assumes assumes ->
    let assumes, scope = visit_assumes assumes scope in
    S_assumes assumes, scope

  | S_requires requires ->
    let requires, scope = visit_requires requires scope in
    S_requires requires, scope

  | S_assigns assigns ->
    let assigns, scope = visit_assigns assigns scope in
    S_assigns assigns, scope

  | S_ensures ensures ->
    let ensures, scope = visit_ensures ensures scope in
    S_ensures ensures, scope

  | S_free free ->
    let free, scope = visit_free free scope in
    S_free free, scope

  | S_warn warn ->
    S_warn warn, scope

let visit_case case scope =
  bind_pair_range case @@ fun case ->
  let body, scope' = visit_list visit_leaf case.case_body scope in
  let case = {
    case_label = case.case_label;
    case_body  = body;
  }
  in
  case, scope

let visit_section sect scope =
  match sect with
  | S_leaf leaf ->
    let leaf, scope = visit_leaf leaf scope in
    S_leaf leaf, scope

  | S_case case ->
    let case, scope = visit_case case scope in
    S_case case, scope

  | S_predicate _ ->
    sect, scope


(** {2 Entry point} *)
(** *************** *)

let doit (stub: stub) : stub =
  bind_range stub @@ fun sections ->
  let scope = Scope.empty in
  let stub', _ = visit_list visit_section sections scope in
  stub'
