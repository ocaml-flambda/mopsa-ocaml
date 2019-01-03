(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** 
   Abstract Syntax Tree for stub specification. Similar to the AST of
   the parser, except for expressions, types and variables for which
   MOPSA counterparts are used. 
*)

open Mopsa
open Universal.Ast
open Format


type stub = {
  stub_name   : string;
  stub_body   : section list;
  stub_params : var list;
  stub_locals  : local with_range list;
  stub_assigns : assigns with_range list;
  stub_return_type : typ option;
  stub_range  : range;
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

and case = {
  case_label     : string;
  case_body      : leaf list;
  case_locals    : local with_range list;
  case_assigns   : assigns with_range list;
  case_range     : range;
}


(** {2 Leaf sections} *)
(** ***************** *)

and requires = formula with_range

and ensures = formula with_range

and assumes = formula with_range

and local = {
  lvar : var;
  lval : local_value;
}

and local_value =
  | L_new of resource
  | L_call of expr (** function *) * expr list (* arguments *)

and assigns = {
  assign_target : expr;
  assign_offset : (expr * expr) list option;
}

and free = expr

and log_binop = C_stubs_parser.Ast.log_binop

and set =
  | S_interval of expr * expr
  | S_resource of resource

and resource = C_stubs_parser.Ast.resource

and builtin = C_stubs_parser.Ast.builtin


(** {2 Formulas} *)
(** ************ *)

and formula =
  | F_expr   of expr
  | F_binop  of log_binop * formula with_range * formula with_range
  | F_not    of formula with_range
  | F_forall of var * set * formula with_range
  | F_exists of var * set * formula with_range
  | F_in     of expr * set


(** {2 Expressions} *)
(*  =-=-=-=-=-=-=-= *)

(** Quantifiers *)
type quant =
  | FORALL
  | EXISTS

type expr_kind +=
  | E_stub_call of stub (** called stub *) * expr list (** arguments *)
  (** Call to a stubbed function *)

  | E_stub_return
  (** Returned value of a stub *)

  | E_stub_builtin_call of builtin * expr
  (** Call to a built-in function *)

  | E_stub_quantified of quant * var * set (** quantified variable over a set of values *)
  (** Quantified variable *)

  | E_stub_attribute of expr * string
  (** Access to an attribute of a resource *)

  | E_stub_resource_mem of expr * resource
  (** Filter environments in which an instance is in a resource pool *)


(** {2 Statements} *)
(*  =-=-=-=-=-=-=- *)

type stmt_kind +=
  | S_stub_free of expr
  (** Release a resource *)

  (** Rename primed variables of assigned dimensions *)
  | S_stub_rename_primed of expr  (** modified pointer *) *
                            (
                              expr  (** index lower bound *) *
                              expr  (** index upper bound *)
                            ) list


(** {2 Heap addresses for resources} *)
(** ******************************** *)

type addr_kind +=
  | A_stub_resource of string (** resource address *)

let () =
  register_addr {
    print = (fun next fmt addr_kind ->
        match addr_kind with
        | A_stub_resource res -> Format.pp_print_string fmt res
        | _ -> next fmt addr_kind
      );
    compare = (fun next ak1 ak2 ->
        match ak1, ak2 with
        | A_stub_resource res1, A_stub_resource res2 -> Pervasives.compare res1 res2
        | _ -> next ak1 ak2
      );
  }


(** {2 Utility functions} *)
(** ********************* *)

let mk_stub_call stub args range =
  mk_expr (E_stub_call (stub, args)) range

let mk_stub_quantified quant v s range =
  mk_expr (E_stub_quantified(quant, v, s)) range ~etyp:v.vtyp

let mk_stub_resource_mem e res range =
  mk_expr (E_stub_resource_mem (e, res)) ~etyp:T_bool range

(** Check whether an expression is quantified? *)
let is_expr_quantified e =
  Visitor.fold_expr
    (fun acc e ->
       match ekind e with
       | E_stub_quantified _ -> Keep true
       | _ -> VisitParts acc
    )
    (fun acc s -> VisitParts acc)
    false
    e


let mk_stub_free e range =
  mk_stmt (S_stub_free e) range

let mk_stub_rename_primed t offsets range =
  mk_stmt (S_stub_rename_primed (t, offsets)) range


(** Visit expressions present in a formula *)
let rec visit_expr_in_formula expr_visitor f =
  bind_range f @@ fun f ->
  match f with
  | F_expr e -> F_expr (Visitor.map_expr expr_visitor (fun stmt -> Keep stmt) e)
  | F_binop (op, f1, f2) -> F_binop (op, visit_expr_in_formula expr_visitor f1, visit_expr_in_formula expr_visitor f2)
  | F_not ff -> F_not (visit_expr_in_formula expr_visitor ff)
  | F_forall (v, s, ff) -> F_forall (v, s, visit_expr_in_formula expr_visitor ff)
  | F_exists (v, s, ff) -> F_exists (v, s, visit_expr_in_formula expr_visitor ff)
  | F_in (v, s) -> F_in (v, s)


let mk_stub_alloc_resource res range =
  mk_alloc_addr (A_stub_resource res) range

let compare_resource = C_stubs_parser.Ast.compare_resource

let compare_set s1 s2 =
  match s1, s2 with
  | S_interval(a1, b1), S_interval(a2, b2) ->
    Compare.compose [
      (fun () -> compare_expr a1 a2);
      (fun () -> compare_expr b1 b2);
    ]

  | S_resource r1, S_resource r2 ->
    compare_resource r1 r2

  | _ ->
    compare s1 s2

(** {2 Pretty printers} *)
(** =-=-=-=-=-=-=-=-=-= *)

let pp_builtin = C_stubs_parser.Ast.pp_builtin

let pp_log_binop = C_stubs_parser.Ast.pp_log_binop

let rec pp_formula fmt f =
  match f.content with
  | F_expr e -> pp_expr fmt e
  | F_binop (op, f1, f2) -> fprintf fmt "(%a)@\n%a (%a)" pp_formula f1 pp_log_binop op pp_formula f2
  | F_not f -> fprintf fmt "not (%a)" pp_formula f
  | F_forall (x, set, f) -> fprintf fmt "∀ %a %a ∈ %a:@ @[<v 2>  %a@]" pp_typ x.vtyp pp_var x pp_set set pp_formula f
  | F_exists (x, set, f) -> fprintf fmt "∃ %a %a ∈ %a:@ @[<v 2>  %a@]" pp_typ x.vtyp pp_var x pp_set set pp_formula f
  | F_in (x, set) -> fprintf fmt "%a ∈ %a" pp_expr x pp_set set

and pp_set fmt =
  function
  | S_interval(e1, e2) -> fprintf fmt "[%a .. %a]" pp_expr e1 pp_expr e2
  | S_resource(r) -> pp_resource fmt r

and pp_resource = C_stubs_parser.Ast.pp_resource

let pp_list pp sep fmt l =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt sep) pp fmt l

let pp_opt pp fmt o =
  match o with
  | None -> ()
  | Some x -> pp fmt x

let rec pp_local fmt local =
  let local = get_content local in
  fprintf fmt "local    : %a %a = @[%a@];"
    pp_typ local.lvar.vtyp
    pp_var local.lvar
    pp_local_value local.lval

and pp_local_value fmt v =
  match v with
  | L_new resource -> fprintf fmt "new %a" pp_resource resource
  | L_call (f, args) -> fprintf fmt "%a(%a)" pp_expr f (pp_list pp_expr ", ") args

let pp_requires fmt requires =
  fprintf fmt "requires : @[%a@];" pp_formula requires.content

let pp_assigns fmt assigns =
  fprintf fmt "assigns  : %a%a;"
    pp_expr assigns.content.assign_target
    (pp_opt (fun fmt l ->
         (pp_print_list ~pp_sep:(fun fmt () -> ())
            (fun fmt (l, u) ->
               fprintf fmt "[%a .. %a]" pp_expr l pp_expr u
            )
         ) fmt l
       )
    ) assigns.content.assign_offset

let pp_assumes fmt (assumes:assumes with_range) =
  fprintf fmt "assumes  : @[%a@];" pp_formula assumes.content

let pp_ensures fmt ensures =
  fprintf fmt "ensures  : @[%a@];" pp_formula ensures.content

let pp_free fmt free =
  fprintf fmt "free : %a;" pp_expr free.content

let pp_leaf_section fmt sec =
  match sec with
  | S_local local -> pp_local fmt local
  | S_assumes assumes -> pp_assumes fmt assumes
  | S_requires requires -> pp_requires fmt requires
  | S_assigns assigns -> pp_assigns fmt assigns
  | S_ensures ensures -> pp_ensures fmt ensures
  | S_free free -> pp_free fmt free

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

let pp_stub fmt stub = pp_sections fmt stub.stub_body


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

        | E_stub_quantified(q1, v1, set1), E_stub_quantified(q2, v2, set2) ->
          Compare.compose [
            (fun () -> compare q1 q2);
            (fun () -> compare_var v1 v2);
            (fun () -> compare_set set1 set2);
          ]

        | E_stub_attribute(o1, f1), E_stub_attribute(o2, f2) ->
          Compare.compose [
            (fun () -> compare_expr o1 o2);
            (fun () -> compare f1 f2)
          ]

        | E_stub_resource_mem(x1, res1), E_stub_resource_mem(x2, res2) ->
          Compare.compose [
            (fun () -> compare_expr x1 x2);
            (fun () -> compare res1 res2);
          ]

        | _ -> next e1 e2
      );

    visit  = (fun next e ->
        match ekind e with
        | E_stub_call _ -> panic "stub visitor not supported"

        | E_stub_return -> leaf e

        | E_stub_builtin_call(f, arg) ->
          { exprs = [arg]; stmts = [] },
          (function {exprs = [arg]} -> {e with ekind = E_stub_builtin_call(f, arg)} | _ -> assert false)

        | E_stub_quantified _ -> leaf e

        | E_stub_attribute(o, f) ->
          { exprs = [o]; stmts = [] },
          (function { exprs = [o] } -> { e with ekind = E_stub_attribute(o, f) } | _ -> assert false)

        | E_stub_resource_mem(x, res) ->
          { exprs = [x]; stmts = []},
          (function { exprs = [x] } -> { e with ekind = E_stub_resource_mem(x, res) } | _ -> assert false)

        | _ -> next e
      );

    print   = (fun next fmt e ->
        match ekind e with
        | E_stub_call (s, args) -> fprintf fmt "stub %s(%a)" s.stub_name (pp_list pp_expr ", ") args
        | E_stub_return -> pp_print_string fmt "return"
        | E_stub_builtin_call(f, arg) -> fprintf fmt "%a(%a)" pp_builtin f pp_expr arg
        | E_stub_quantified(FORALL, v, _) -> fprintf fmt "∀%a" pp_var v
        | E_stub_quantified(EXISTS, v, _) -> fprintf fmt "∃%a" pp_var v
        | E_stub_attribute(o, f) -> fprintf fmt "%a:%s" pp_expr o f
        | E_stub_resource_mem(x, res) -> fprintf fmt "%a ∈ %a" pp_expr x pp_resource res
        | _ -> next fmt e
      );
  }

(** {2 Registration of statements} *)
(*  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

let () =
  register_stmt {
    compare = (fun next s1 s2 ->
        match skind s1, skind s2 with
        | S_stub_free(e1), S_stub_free(e2) ->
          compare_expr e1 e2

        | S_stub_rename_primed(t1, offsets1), S_stub_rename_primed(t2, offsets2) ->
          Compare.compose [
            (fun () -> compare_expr t1 t2);
            (fun () -> Compare.list (Compare.pair compare_expr compare_expr) offsets1 offsets2);
          ]

        | _ -> next s1 s2
      );

    visit = (fun next s ->
        match skind s with
        | S_stub_free e ->
          { exprs = [e]; stmts = [] },
          (function { exprs = [e] } -> { s with skind = S_stub_free e } | _ -> assert false)

        | S_stub_rename_primed(t, offsets) -> panic "visitor for S_stub_rename_primed not supported"

        | _ -> next s
      );

    print = (fun next fmt s ->
        match skind s with
        | S_stub_free e -> fprintf fmt "free(%a);" pp_expr e

        | S_stub_rename_primed(t,offsets) ->
          fprintf fmt "rename primed %a%a;"
            pp_expr t
            (pp_print_list
               ~pp_sep:(fun fmt () -> ())
               (fun fmt (a, b) ->
                  fprintf fmt "[%a .. %a]"
                    pp_expr a
                    pp_expr b
               )
            ) offsets
        | _ -> next fmt s
      );
  }
