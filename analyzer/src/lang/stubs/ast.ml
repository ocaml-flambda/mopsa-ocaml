(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstract Syntax Tree for stubs specifications. *)

open Framework.Essentials
open Format

(** {2 Types} *)
(*  =-=-=-=-= *)

type quantifier =
  | FORALL
  | EXISTS


type typ +=
  | T_stub_predicate (** predicate *)
  | T_stub_quant_var of quantifier * typ

(** {2 Operators} *)
(*  =-=-=-=-=-=-= *)

type operator +=
  | O_log_implies (** ⇒ *)


(** {2 Expressions} *)
(*  =-=-=-=-=-=-=-= *)


(** Built-in functions *)
type builtin =
  | SIZE
  | OFFSET
  | BASE
  | OLD

let pp_builtin fmt f =
  match f with
  | SIZE   -> pp_print_string fmt "size"
  | OFFSET -> pp_print_string fmt "offset"
  | BASE   -> pp_print_string fmt "base"
  | OLD    -> pp_print_string fmt "old"


type expr_kind +=
  | E_stub_return
  | E_stub_builtin of builtin


(** {2 Formulas} *)
(*  =-=-=-=-=-=- *)

type 'a with_range = {
  kind: 'a;
  range: range;
}

type formula =
  | F_expr   of expr
  | F_bool   of bool
  | F_binop  of operator * formula with_range * formula with_range
  | F_not    of formula with_range
  | F_forall of var * set * formula with_range
  | F_exists of var * set * formula with_range
  | F_in     of var * set
  | F_free   of expr

and set =
  | S_interval of expr * expr
  | S_resource of resource

and resource = {
  res_name : string;
  res_uid  : int;
}


(** {2 Statements} *)
(*  =-=-=-=-=-=-=- *)

type stub =
  | S_simple of simple_stub
  | S_case  of case_stub

and simple_stub = {
  sstub_predicates : predicate with_range list;
  sstub_requires   : requires with_range list;
  sstub_assigns    : assigns with_range list;
  sstub_local      : local with_range list;
  sstub_ensures    : ensures with_range list;
}

and case_stub = {
  cstub_predicates : predicate with_range list;
  cstub_requires   : requires with_range list;
  cstub_cases      : case with_range list;
}

and predicate = {
  pred_var : var;
  pred_body: formula with_range;
}

and requires = formula with_range
and ensures = formula with_range
and assumes = formula with_range

and local = {
  local_var : var;
  local_value : local_value;
}

and local_value =
  | Local_new           of resource
  | Local_function_call of var (** function *) * expr list (* arguments *)

and assigns = {
  assigns_target: expr;
  assigns_itv: (expr * expr) option;
}

and case = {
  case_label: string;
  case_assumes: assumes with_range list;
  case_requires : requires with_range list;
  case_assigns  : assigns with_range list;
  case_local    : local with_range list;
  case_ensures  : ensures with_range list;
}


type stmt_kind +=
  | S_stub of stub



(** {2 Pretty printers} *)
(** =-=-=-=-=-=-=-=-=-= *)

let pp_list pp sep fmt l =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt sep) pp fmt l

let pp_opt pp fmt o =
  match o with
  | None -> ()
  | Some x -> pp fmt x

let rec pp_formula fmt f =
  match f.kind with
  | F_expr e -> pp_expr fmt e
  | F_bool true  -> pp_print_string fmt "true"
  | F_bool false -> pp_print_string fmt "false"
  | F_binop (op, f1, f2) -> fprintf fmt "(%a) %a (%a)" pp_formula f1 pp_operator op pp_formula f2
  | F_not f -> fprintf fmt "not (%a)" pp_formula f
  | F_forall (x, set, f) -> fprintf fmt "∀ %a %a ∈ %a: @[%a@]" pp_typ x.vtyp pp_var x pp_set set pp_formula f
  | F_exists (x, set, f) -> fprintf fmt "∃ %a %a ∈ %a: @[%a@]" pp_typ x.vtyp pp_var x pp_set set pp_formula f
  | F_in (x, set) -> fprintf fmt "%a ∈ %a" pp_var x pp_set set
  | F_free e -> fprintf fmt "free(%a)" pp_expr e

and pp_set fmt =
  function
  | S_interval(e1, e2) -> fprintf fmt "[%a .. %a]" pp_expr e1 pp_expr e2
  | S_resource(r) -> pp_resource fmt r

and pp_resource fmt res =
  pp_print_string fmt res.res_name

let rec pp_local fmt local =
  fprintf fmt "local: %a %a = @[%a@];"
    pp_typ local.kind.local_var.vtyp
    pp_var local.kind.local_var
    pp_local_value local.kind.local_value

and pp_local_value fmt v =
  match v with
  | Local_new resouce -> fprintf fmt "new %a" pp_resource resouce
  | Local_function_call (f, args) -> fprintf fmt "%a(%a)" pp_var f (pp_list pp_expr ", ") args


let pp_predicate fmt pred =
  fprintf fmt "predicate %a: @[%a@];"
    pp_var pred.kind.pred_var
    pp_formula pred.kind.pred_body

let pp_requires fmt requires =
  fprintf fmt "requires: @[%a@];"
    pp_formula requires.kind

let pp_assigns fmt assigns =
  fprintf fmt "assigns: %a%a;"
    pp_expr assigns.kind.assigns_target
    (pp_opt (fun fmt (l, u) ->
         fprintf fmt "[%a .. %a]" pp_expr l pp_expr u
       )
    ) assigns.kind.assigns_itv


let pp_assumes fmt assumes =
  fprintf fmt "assumes: @[%a@];" pp_formula assumes.kind

let pp_ensures fmt ensures =
  fprintf fmt "ensures: @[%a@];" pp_formula ensures.kind

let pp_case fmt case =
  fprintf fmt "case \"%s\":@\n  @[%a%a%a%a%a@]"
    case.kind.case_label
    (pp_list pp_assumes "@\n") case.kind.case_assumes
    (pp_list pp_local "@\n") case.kind.case_local
    (pp_list pp_requires "@\n") case.kind.case_requires
    (pp_list pp_assigns "@\n") case.kind.case_assigns
    (pp_list pp_ensures "@\n") case.kind.case_ensures

let pp_stub fmt stub =
  match stub with
  | S_simple ss ->
    fprintf fmt "%a%a%a%a%a"
      (pp_list pp_predicate "@\n") ss.sstub_predicates
      (pp_list pp_requires "@\n") ss.sstub_requires
      (pp_list pp_assigns "@\n") ss.sstub_assigns
      (pp_list pp_local "@\n") ss.sstub_local
      (pp_list pp_ensures "@\n") ss.sstub_ensures

  | S_case cs ->
    fprintf fmt "%a%a%a"
      (pp_list pp_predicate "@\n") cs.cstub_predicates
      (pp_list pp_requires "@\n") cs.cstub_requires
      (pp_list pp_case "@\n") cs.cstub_cases


(** {2 AST registration} *)
(*  =-=-=-=-=-=-=-=-=-=- *)

let () =
  register_typ {
    compare = (fun next t1 t2 ->
        match t1, t2 with
        | T_stub_predicate, T_stub_predicate -> 0
        | T_stub_quant_var(q1, t1), T_stub_quant_var(q2, t2) ->
          Compare.compose [
            (fun () -> compare q1 q2);
            (fun () -> compare_typ t1 t2);
          ]
        | _ -> next t1 t2
      );

    print = (fun next fmt t ->
        match t with
        | T_stub_predicate -> fprintf fmt "predicate"
        | T_stub_quant_var(FORALL, t') -> fprintf fmt "∀%a" pp_typ t'
        | T_stub_quant_var(EXISTS, t') -> fprintf fmt "∃%a" pp_typ t'
        | _ -> next fmt t
      );
  }

let () =
  register_operator {
    compare = (fun next -> next);
    print   = (fun next fmt o ->
        match o with
        | O_log_implies -> Format.fprintf fmt "⇒"
        | _ -> next fmt o
      );
  }

let () =
  register_expr {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_stub_builtin(f1), E_stub_builtin(f2) -> compare f1 f2
        | _ -> next e1 e2
      );
    visit   = (fun next e ->
        match ekind e with
        | E_stub_return -> Framework.Visitor.leaf e
        | E_stub_builtin _ -> Framework.Visitor.leaf e
        | _ -> next e
      );
    print   = (fun next fmt e ->
        match ekind e with
        | E_stub_return -> pp_print_string fmt "return"
        | E_stub_builtin f -> pp_builtin fmt f
        | _ -> next fmt e
      );
  }

let () =
  register_stmt {
    compare = (fun next s1 s2 ->
        match skind s1, skind s2 with
        | S_stub _, S_stub _ -> compare_range (srange s1) (srange s2)
        | _ -> next s1 s2
      );

    print = (fun next fmt s ->
        match skind s with
        | S_stub s -> pp_stub fmt s
        | _ -> next fmt s
      );

    visit = (fun next s ->
        match skind s with
        | S_stub _ -> Framework.Visitor.leaf s
        | _ -> next s
      );
  }
