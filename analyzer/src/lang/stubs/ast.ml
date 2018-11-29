(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstract Syntax Tree for stub specification. *)

open Framework.Essentials
open Format


(** {2 Formulas} *)
(*  =-=-=-=-=-=- *)

(** Logical binary operators *)
type log_binop =
  | AND     (** ∧ *)
  | OR      (** ∨ *)
  | IMPLIES (** ⟹ *)


(** Built-in functions *)
type builtin =
  | SIZE
  | OFFSET
  | BASE
  | OLD


type formula =
  | F_expr   of expr (** boolean expression *)
  | F_binop  of log_binop * formula with_range * formula with_range
  | F_not    of formula with_range
  | F_forall of var * set * formula with_range
  | F_exists of var * set * formula with_range
  | F_in     of var * set (* set membership predicate *)
  | F_free   of expr (* resource release *)

and set =
  | S_interval of expr * expr (* intervals of integers  *)
  | S_resource of resource    (* set of allocated instances of a resource *)

and resource = string (* FIXME: add an uid *)

let mk_stub_and f1 f2 range =
  with_range (F_binop (AND, f1, f2)) range

(** {2 Stubs} *)
(*  =-=-=-=-= *)

(** A stub is composed of pre-conditions requirements and a body. *)
type stub = {
  stub_name: string;
  stub_requires: requires with_range list;
  stub_params: var list;
  stub_return_type: typ option;
  stub_body: body;
}

(** Body of a stub *)
and body =
  | B_post of post
  (** A simple stub body composed of only one post-condition *)

  | B_cases   of case with_range list
  (** A composed stub containing a number of cases *)

(** Post-conditions give the modified variables, the value of locals
    and the ensured conditions on the output state. *)
and post = {
  post_assigns  : assigns with_range list;
  post_local    : local with_range list;
  post_ensures  : ensures with_range list;
}

(** A stub case consists of a filter on the pre-condition and a
   post-condition. The assumes section is equivalent to an `if`
   statement, while the requires section is equivalent to an `assert`
   statement. *)
and case = {
  case_label    : string;
  case_assumes  : assumes with_range list;
  case_requires : requires with_range list;
  case_post : post;
}


(* The sections requires, assumes and ensures are just logic formula *)
and requires = formula with_range
and ensures  = formula with_range
and assumes  = formula with_range

(* Local variables have values in the post-condition only. *)
and local = {
  lvar   : var;
  lval : local_value;
}

(* Values of local variables *)
and local_value =
  | L_new  of resource         (* allocation of a resource *)
  | L_call of expr * expr list (* call to a function *)


(* Assigned memory regions are declared in the assigns section *)
and assigns = {
  assign_target: expr;                 (* assigned memory block *)
  assign_offset: (expr * expr) option; (* range of modified indices *)
}

(** {2 Expressions} *)
(*  =-=-=-=-=-=-=-= *)

type quantifier =
  | FORALL
  | EXISTS

type expr_kind +=
  | E_stub_call of stub (** called stub *) * expr list (** arguments *)
  | E_stub_return
  | E_stub_builtin_call of builtin with_range * expr
  | E_stub_quantified of quantifier * expr (** quantified expression *)

let mk_stub_call stub args range =
  mk_expr (E_stub_call (stub, args)) range

let mk_stub_quantified quant exp range = mk_expr (E_stub_quantified(quant, exp)) range ~etyp:exp.etyp


(** {2 Statements} *)
(*  =-=-=-=-=-=-=- *)

type stmt_kind +=
  | S_stub_assigns of expr                 (** modified target *) *
                      (expr * expr) option (** modification range *)
  (** Declare an assignment and create old copies *)

  | S_stub_remove_old of expr
  (** remove old copies of an assigned target *)


let mk_stub_assigns target offsets range =
  mk_stmt (S_stub_assigns (target, offsets)) range

let mk_stub_remove_old target range =
  mk_stmt (S_stub_remove_old target) range


(** {2 Pretty printers} *)
(** =-=-=-=-=-=-=-=-=-= *)

let pp_opt pp fmt o =
  match o with
  | None -> ()
  | Some x -> pp fmt x

let pp_builtin fmt f =
  match f with
  | SIZE   -> pp_print_string fmt "size"
  | OFFSET -> pp_print_string fmt "offset"
  | BASE   -> pp_print_string fmt "base"
  | OLD    -> pp_print_string fmt "old"

let pp_log_binop fmt = function
  | AND -> pp_print_string fmt "∧"
  | OR  -> pp_print_string fmt "∨"
  | IMPLIES -> pp_print_string fmt "⟹"

let rec pp_formula fmt f =
  match f.content with
  | F_expr e -> pp_expr fmt e
  | F_binop (op, f1, f2) -> fprintf fmt "(%a)@\n%a (%a)" pp_formula f1 pp_log_binop op pp_formula f2
  | F_not f -> fprintf fmt "not (%a)" pp_formula f
  | F_forall (x, set, f) -> fprintf fmt "∀ %a %a ∈ %a:@ @[<v 2>  %a@]" pp_typ x.vtyp pp_var x pp_set set pp_formula f
  | F_exists (x, set, f) -> fprintf fmt "∃ %a %a ∈ %a:@ @[<v 2>  %a@]" pp_typ x.vtyp pp_var x pp_set set pp_formula f
  | F_in (x, set) -> fprintf fmt "%a ∈ %a" pp_var x pp_set set
  | F_free e -> fprintf fmt "free(%a)" pp_expr e

and pp_set fmt =
  function
  | S_interval(e1, e2) -> fprintf fmt "[%a .. %a]" pp_expr e1 pp_expr e2
  | S_resource(r) -> pp_resource fmt r

and pp_resource fmt res = pp_print_string fmt res

let pp_args pp fmt args =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp fmt args

let rec pp_local fmt local =
  fprintf fmt "local: %a %a = @[%a@];"
    pp_typ local.content.lvar.vtyp
    pp_var local.content.lvar
    pp_local_value local.content.lval

and pp_local_value fmt v =
  match v with
  | L_new resouce -> fprintf fmt "new %a" pp_resource resouce
  | L_call (f, args) -> fprintf fmt "%a(%a)" pp_expr f (pp_args pp_expr) args


let pp_requires fmt requires =
  fprintf fmt "requires:@ @[<v 2>  %a@];"
    pp_formula requires.content

let pp_assigns fmt assigns =
  fprintf fmt "assigns:@ @[<v 2>  %a%a@];"
    pp_expr assigns.content.assign_target
    (pp_opt (fun fmt (l, u) ->
         fprintf fmt "[%a .. %a]" pp_expr l pp_expr u
       )
    ) assigns.content.assign_offset


let pp_assumes fmt assumes =
  fprintf fmt "assumes:@ @[<v 2>  %a@];" pp_formula assumes.content

let pp_ensures fmt ensures =
  fprintf fmt "ensures:@ @[<v 2>  %a@];" pp_formula ensures.content

let pp_section pp ?(first=false) fmt l =
  if not first && l != [] then fprintf fmt "@\n";
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") pp fmt l


let pp_post ~first fmt post =
  fprintf fmt "%a%a%a"
    (pp_section pp_assigns ~first) post.post_assigns
    (pp_section pp_local ~first:(first && post.post_assigns == [])) post.post_local
    (pp_section pp_ensures ~first:(first && post.post_assigns == [] && post.post_local == [])) post.post_ensures

let pp_case fmt case =
  fprintf fmt "case \"%s\":@\n  @[%a%a%a@]"
    case.content.case_label
    (pp_section pp_assumes ~first:true) case.content.case_assumes
    (pp_section pp_requires ~first:(case.content.case_assumes == [])) case.content.case_requires
    (pp_post ~first:(case.content.case_assumes == [] && case.content.case_requires == [])) case.content.case_post

let pp_body ~first fmt body =
  match body with
  | B_post post -> pp_post ~first fmt post
  | B_cases cases  -> (pp_section pp_case ~first) fmt cases

let pp_stub fmt stub =
  fprintf fmt "%a%a"
    (pp_section pp_requires ~first:true) stub.stub_requires
    (pp_body ~first:(stub.stub_requires == [])) stub.stub_body


(** {2 Registration of expressions} *)
(*  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= *)

let () =
  register_expr {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_stub_call _, E_stub_call _ -> panic "stub comparison not supported"

        | E_stub_builtin_call(f1, arg1), E_stub_builtin_call(f2, arg2) ->
          Compare.compose [
            (fun () -> compare f1 f2);
            (fun () -> compare_expr arg1 arg2)
          ]

        | E_stub_quantified(q1, ee1), E_stub_quantified(q2, ee2) ->
          Compare.compose [
            (fun () -> compare q1 q2);
            (fun () -> compare_expr ee1 ee2);
          ]

        | _ -> next e1 e2
      );

    visit  = (fun next e ->
        match ekind e with
        | E_stub_call _ -> panic "stub visitor not supported"

        | E_stub_return -> Framework.Visitor.leaf e

        | E_stub_builtin_call(f, arg) ->
          { exprs = [arg]; stmts = [] },
          (function {exprs = [args]} -> {e with ekind = E_stub_builtin_call(f, arg)} | _ -> assert false)

        | E_stub_quantified(q, ee) ->
          { exprs = [ee]; stmts = [] },
          (function {exprs = [ee]} -> {e with ekind = E_stub_quantified(q, ee)} | _ -> assert false)

        | _ -> next e
      );

    print   = (fun next fmt e ->
        match ekind e with
        | E_stub_call (s, args) -> fprintf fmt "stub %s(%a)" s.stub_name (pp_args pp_expr) args
        | E_stub_return -> pp_print_string fmt "return"
        | E_stub_builtin_call(f, arg) -> fprintf fmt "%a(%a)" pp_builtin f.content pp_expr arg
        | E_stub_quantified(FORALL, ee) -> fprintf fmt "∀%a" pp_expr ee
        | E_stub_quantified(EXISTS, ee) -> fprintf fmt "∃%a" pp_expr ee
        | _ -> next fmt e
      );
  }

(** {2 Registration of statements} *)
(*  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

let () =
  register_stmt {
    compare = (fun next s1 s2 ->
        match skind s1, skind s2 with
        | S_stub_assigns(t1, o1), S_stub_assigns(t2, o2) ->
          Compare.compose [
            (fun () -> compare_expr t1 t2);
            (fun () -> Compare.option (Compare.pair compare_expr compare_expr) o1 o2)
          ]

        | S_stub_remove_old(t1), S_stub_remove_old(t2) ->
          compare_expr t1 t2

        | _ -> next s1 s2
      );

    visit = (fun next s ->
        match skind s with
        | S_stub_assigns(t, o) -> panic "visitor for S_stub_assigns not supported"
        | S_stub_remove_old(t) -> panic "visitor for S_stub_remove_old not supported"
        | _ -> next s
      );

    print = (fun next fmt s ->
        match skind s with
        | S_stub_assigns(t, None) -> fprintf fmt "assigns: %a;" pp_expr t
        | S_stub_assigns(t, Some(a,b)) -> fprintf fmt "assigns: %a[%a .. %a];" pp_expr t pp_expr a pp_expr b
        | S_stub_remove_old(target) -> fprintf fmt "remove old(%a);" pp_expr target
        | _ -> next fmt s
      );
  }


(** {2 Visitors} *)
(** =-=-=-=-=-=- *)

let rec visit_expr_in_formula expr_visitor f =
  bind_range f @@ fun f ->
  match f with
  | F_expr e -> F_expr (Visitor.map_expr expr_visitor (fun stmt -> stmt) e)
  | F_binop (op, f1, f2) -> F_binop (op, visit_expr_in_formula expr_visitor f1, visit_expr_in_formula expr_visitor f2)
  | F_not ff -> F_not (visit_expr_in_formula expr_visitor ff)
  | F_forall (v, s, ff) -> F_forall (v, s, visit_expr_in_formula expr_visitor ff)
  | F_exists (v, s, ff) -> F_exists (v, s, visit_expr_in_formula expr_visitor ff)
  | F_in (v, s) -> F_in (v, s)
  | F_free e -> F_free (expr_visitor e)
