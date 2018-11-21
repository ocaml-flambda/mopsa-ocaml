(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inline predicates similarly to macro expansion. At the end,
    formulas in the CST should not contain any predicate. 
*)

open Location
open Cst


(** {2 Inlining context} *)
(** -------------------- *)

module Context =
struct

  (** Context maps give for every predicate its inlined body formula *)
  module PredMap = Map.Make
      (
      struct
        type t = predicate
        let compare p1 p2 = compare_var p1.predicate_var p2.predicate_var
      end
      )

  type t = formula PredMap.t

  let empty : t = PredMap.empty

  let add (pred:predicate) (body:formula) (ctx:t) : t =
    PredMap.add pred body ctx

  let inline (pred:var) (params:expr with_range list) (ctx:t) range : formula with_range =
    (* We need to find the predicate record containing the names of the arguments *)
    let pred, body = PredMap.find_first (fun p -> compare_var p.predicate_var pred == 0) ctx in

    (* Combine arguments and call parameters *)
    let args = List.map get_content params |>
               List.combine pred.predicate_args
    in
    let find_arg_expr arg =
      let rec iter =
        function
        | [] -> None
        | (v,e) :: tl ->
          if compare_var arg v == 0 then Some e
          else iter tl
      in
      iter args
    in

    (* Replace occurrences of args in body by params *)
    let rec visit_list visit l =
      match l with
      | [] -> []
      | hd :: tl ->
        let hd' = visit hd in
        let tl' = visit_list visit tl in
        hd' :: tl'
    in

    let rec visit_formula f =
      bind_range f @@ fun f ->
      match f with
      | F_expr e -> F_expr (visit_expr e)
      | F_binop (op, f1, f2) -> F_binop(op, visit_formula f1, visit_formula f2)
      | F_not f -> F_not (visit_formula f)
      | F_forall(v, t, s, f) -> F_forall(v, t, s, visit_formula f)
      | F_exists(v, t, s, f) -> F_exists(v, t, s, visit_formula f)
      | F_predicate(p, params) -> F_predicate(p, visit_list visit_expr params)
      | F_free e -> F_free (visit_expr e)
      | F_in _ | F_bool _ -> f

    and visit_expr (e:expr with_range) =
      bind_range e @@ fun e ->
      match e with
      | E_int _ | E_float _ | E_string _ | E_char _ | E_return -> e
      | E_var v ->
        begin match find_arg_expr v with
          | None -> e
          | Some e -> e
        end
      | E_unop(op, e) -> E_unop(op, visit_expr e)
      | E_binop (op, e1, e2) -> E_binop(op, visit_expr e1, visit_expr e2)
      | E_addr_of e -> E_addr_of (visit_expr e)
      | E_deref e -> E_deref (visit_expr e)
      | E_cast (t, e) -> E_cast(t, visit_expr e)
      | E_subscript (a, i) -> E_subscript(visit_expr a, visit_expr i)
      | E_member (s, f) -> E_member(visit_expr s, f)
      | E_arrow (p, f) -> E_arrow(visit_expr p, f)
      | E_builtin_call (f, e) -> E_builtin_call(f, visit_expr e)
    in

    visit_formula (with_range body range)
    
end

(** Generic visitor on lists *)
let rec visit_list visit l ctx =
  match l with
  | [] -> []
  | hd :: tl ->
    let hd' = visit hd ctx in
    let tl' = visit_list visit tl ctx in
    hd' :: tl'

let rec visit_formula f ctx : formula with_range =
  let range = get_range f in
  bind_range f @@ fun f ->
  match f with
  | F_expr _ | F_bool _ | F_in (_, _) | F_free _ -> f

  | F_predicate (p, params) ->
    let f = Context.inline p params ctx range in
    visit_formula f ctx |>
    get_content

  | F_binop (op, f1, f2) -> F_binop (op, visit_formula f1 ctx, visit_formula f2 ctx)
  | F_not f -> F_not (visit_formula f ctx)
  | F_forall (v, t, s, f) -> F_forall (v, t, s, visit_formula f ctx)
  | F_exists (v, t, s, f) -> F_exists (v, t, s, visit_formula f ctx)


let update_context pred ctx =
  let body = visit_formula pred.content.predicate_body ctx in
  Context.add pred.content body.content ctx

let visit_requires req ctx =
  bind_range req @@ fun req ->
  visit_formula req ctx

let visit_assumes a ctx =
  bind_range a @@ fun a ->
  visit_formula a ctx

let visit_ensures e ctx =
  bind_range e @@ fun e ->
  visit_formula e ctx

let visit_case case ctx =
  bind_range case @@ fun case ->
  {
    case with
    case_assumes = visit_list visit_assumes case.case_assumes ctx;
    case_requires = visit_list visit_requires case.case_requires ctx;
    case_ensures = visit_list visit_ensures case.case_ensures ctx;
  }

let rec doit (stub:stub with_range) : stub with_range =
  bind_range stub @@ fun stub ->
  match stub with
  | S_simple s ->
    let ctx = List.fold_left (fun ctx pred ->
        update_context pred ctx
      ) Context.empty s.simple_stub_predicates
    in
    let requires = visit_list visit_requires s.simple_stub_requires ctx in
    let ensures = visit_list visit_requires s.simple_stub_ensures ctx in
    S_simple {
      s with
      simple_stub_predicates = [];
      simple_stub_requires = requires;
      simple_stub_ensures = ensures;
    }

  | S_case s ->
    let ctx = List.fold_left (fun ctx pred ->
        update_context pred ctx
      ) Context.empty s.case_stub_predicates
    in
    let requires = visit_list visit_requires s.case_stub_requires ctx in
    let cases = visit_list visit_case s.case_stub_cases ctx in
    S_case {
      case_stub_predicates = [];
      case_stub_requires = requires;
      case_stub_cases = cases;
    }

