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
   Abstract Syntax Tree for stub specification. Similar to the AST of
   the parser, except for expressions, types and variables for which
   MOPSA counterparts are used.
*)

open Mopsa
open Universal.Ast
open Format


(** Stub for a function *)
type stub_func = {
  stub_func_name   : string;
  stub_func_body   : section list;
  stub_func_params : var list;
  stub_func_locals  : local with_range list;
  stub_func_assigns : assigns with_range list;
  stub_func_return_type : typ option;
  stub_func_range  : range;
}

(** Stub for a global directive *)
and stub_directive = {
  stub_directive_body : section list;
  stub_directive_locals : local with_range list;
  stub_directive_assigns : assigns with_range list;
  stub_directive_range : range;
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
  assign_offset : (expr * expr) list;
}

and free = expr

and warn = string

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


let compare_assigns a1 a2 =
  Compare.pair compare_expr (Compare.list (Compare.pair compare_expr compare_expr))
    (a1.assign_target, a2.assign_offset) (a2.assign_target, a2.assign_offset)

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

let rec compare_formula f1 f2 =
  match f1.content, f2.content with
  | F_expr e1, F_expr e2 -> compare_expr e1 e2
  | F_binop(o1, f1, g1), F_binop(o2, f2, g2) ->
    Compare.compose [
      (fun () -> compare o1 o2);
      (fun () -> compare_formula f1 f2);
      (fun () -> compare_formula g1 g2);
    ]
  | F_not f1, F_not f2 -> compare_formula f1 f2
  | F_forall(v1,s1,f1), F_forall(v2,s2,f2) ->
    Compare.compose [
      (fun () -> compare_var v1 v2);
      (fun () -> compare_set s1 s2);
      (fun () -> compare_formula f1 f2);
    ]
  | F_exists(v1,s1,f1), F_exists(v2,s2,f2) ->
    Compare.compose [
      (fun () -> compare_var v1 v2);
      (fun () -> compare_set s1 s2);
      (fun () -> compare_formula f1 f2);
    ]
  | F_in(e1,s1), F_in(e2,s2) ->
    Compare.compose [
      (fun () -> compare_expr e1 e2);
      (fun () -> compare_set s1 s2)
    ]
  | x,y -> compare x y


(** {2 Expressions} *)
(*  =-=-=-=-=-=-=-= *)

(** Quantifiers *)
type quant =
  | FORALL
  | EXISTS

type expr_kind +=
  | E_stub_call of stub_func (** called stub *) * expr list (** arguments *)
  (** Call to a stubbed function *)

  | E_stub_return
  (** Returned value of a stub *)

  | E_stub_builtin_call of builtin * expr
  (** Call to a built-in function *)

  | E_stub_quantified of quant * var * set (** quantified variable over a set of values *)
  (** Quantified variable *)

  | E_stub_attribute of expr * string
  (** Access to an attribute of a resource *)

  | E_stub_alloc of string
  (** Allocate a resource and translate it into an address *)

  | E_stub_resource_mem of expr * resource
  (** Filter environments in which an instance is in a resource pool *)

  | E_stub_primed of expr
  (** Primed expressions denoting values in the post-state *)



(** {2 Statements} *)
(*  =-=-=-=-=-=-=- *)

type stmt_kind +=
  | S_stub_directive of stub_directive
  (** A call to a stub directive, which are stub functions called at the
      initialization of the analysis. Useful to initialize variables with stub
      formulas.
  *)

  | S_stub_free of expr
  (** Release a resource *)

  | S_stub_requires of expr
  (** Filter the current environments that verify a condition, and raise an
      alarm if the condition may be violated. *)


  | S_stub_prepare_all_assigns of assigns list
  (** Prepare primed copies of assigned objects *)


  | S_stub_assigns of assigns
  (** Declare an assigned object *)


  | S_stub_clean_all_assigns of assigns list
  (** Clean the post-state of primed copies *)


(** {2 Heap addresses for resources} *)
(** ******************************** *)

type addr_kind +=
  | A_stub_resource of string (** resource address *)

let () =
  register_addr_kind {
    print = (fun next fmt addr_kind ->
        match addr_kind with
        | A_stub_resource res -> Format.pp_print_string fmt res
        | _ -> next fmt addr_kind
      );
    compare = (fun next ak1 ak2 ->
        match ak1, ak2 with
        | A_stub_resource res1, A_stub_resource res2 -> Stdlib.compare res1 res2
        | _ -> next ak1 ak2
      );
  }


(** {2 Utility functions} *)
(** ********************* *)

let mk_stub_call stub args range =
  let t = match stub.stub_func_return_type with
    | None -> T_any
    | Some t -> t
  in
  mk_expr (E_stub_call (stub, args)) ~etyp:t range

let mk_stub_quantified quant v s range =
  mk_expr (E_stub_quantified(quant, v, s)) range ~etyp:v.vtyp

let mk_stub_resource_mem e res range =
  mk_expr (E_stub_resource_mem (e, res)) ~etyp:T_bool range

let mk_stub_primed e range =
  mk_expr (E_stub_primed e) ~etyp:e.etyp range


(** Check whether an expression is quantified? *)
let is_expr_quantified quant e =
  Visitor.fold_expr
    (fun acc e ->
       match ekind e with
       | E_stub_quantified (q,_,_) -> Keep (q = quant)
       | _ -> VisitParts acc
    )
    (fun acc s -> VisitParts acc)
    false
    e


(** Check whether an expression is universally quantified? *)
let is_expr_forall_quantified e =
  is_expr_quantified FORALL e

(** Check whether an expression is existentially quantified? *)
let is_expr_exists_quantified e =
  is_expr_quantified EXISTS e

let mk_stub_directive stub range =
  mk_stmt (S_stub_directive stub) range

let mk_stub_free e range =
  mk_stmt (S_stub_free e) range

let mk_stub_prepare_all_assigns assigns range =
  mk_stmt (S_stub_prepare_all_assigns (List.map get_content assigns)) range

let mk_stub_assigns t offset range =
  mk_stmt (S_stub_assigns { assign_target = t; assign_offset = offset}) range

let mk_stub_clean_all_assigns assigns range =
  mk_stmt (S_stub_clean_all_assigns (List.map get_content assigns)) range

let mk_stub_requires cond range =
  mk_stmt (S_stub_requires cond) range

let is_stub_primed e =
  fold_expr
    (fun acc ee ->
       match ekind ee with
       | E_stub_primed _ -> Keep true
       | _ -> VisitParts acc
    )
    (fun acc stmt -> VisitParts acc)
    false e

(** Visit expressions present in a formula *)
let rec visit_expr_in_formula visitor f =
  bind_range f @@ fun f ->
  match f with
  | F_expr e -> F_expr (visit_expr visitor e)
  | F_binop (op, f1, f2) -> F_binop (op, visit_expr_in_formula visitor f1, visit_expr_in_formula visitor f2)
  | F_not ff -> F_not (visit_expr_in_formula visitor ff)
  | F_forall (v, s, ff) -> F_forall (v, visit_set visitor s, visit_expr_in_formula visitor ff)
  | F_exists (v, s, ff) -> F_exists (v, visit_set visitor s, visit_expr_in_formula visitor ff)
  | F_in (e, s) -> F_in (visit_expr visitor e, visit_set visitor s)

and visit_set visitor s =
  match s with
  | S_interval (e1, e2) -> S_interval (visit_expr visitor e1, visit_expr visitor e2)
  | S_resource r -> S_resource r

and visit_expr visitor e =
  Visitor.map_expr visitor (fun stmt -> Keep stmt) e

let mk_stub_alloc_resource res range =
  mk_expr (E_stub_alloc res) range


let negate_log_binop : log_binop -> log_binop = function
  | AND -> OR
  | OR -> AND
  | IMPLIES -> assert false


let flip_quantified_var v s f =
  visit_expr_in_formula
    (fun e ->
       match ekind e with
       | E_stub_quantified(FORALL, vv, s) when compare_var v vv = 0 ->
         VisitParts { e with ekind = E_stub_quantified(EXISTS, v, s) }

       | E_stub_quantified(EXISTS, vv, s) when compare_var v vv = 0 ->
         VisitParts { e with ekind = E_stub_quantified(FORALL, v, s) }

       | _ -> VisitParts e
    ) f

let flip_quantified_vars s f =
  visit_expr_in_formula
    (fun e ->
       match ekind e with
       | E_stub_quantified(FORALL, v, s) ->
         VisitParts { e with ekind = E_stub_quantified(EXISTS, v, s) }

       | E_stub_quantified(EXISTS, v, s) ->
         VisitParts { e with ekind = E_stub_quantified(FORALL, v, s) }

       | _ -> VisitParts e
    ) f


(** {2 Pretty printers} *)
(** =-=-=-=-=-=-=-=-=-= *)

let pp_builtin = C_stubs_parser.Ast.pp_builtin

let pp_log_binop = C_stubs_parser.Ast.pp_log_binop

let rec pp_formula fmt f =
  match f.content with
  | F_expr e -> pp_expr fmt e
  | F_binop (op, f1, f2) -> fprintf fmt "(%a)@,%a (%a)" pp_formula f1 pp_log_binop op pp_formula f2
  | F_not f -> fprintf fmt "not (%a)" pp_formula f
  | F_forall (x, set, f) -> fprintf fmt "@[<v 2>∀ %a %a ∈ %a:@,%a@]" pp_typ x.vtyp pp_var x pp_set set pp_formula f
  | F_exists (x, set, f) -> fprintf fmt "@[<v 2>∃ %a %a ∈ %a:@,%a@]" pp_typ x.vtyp pp_var x pp_set set pp_formula f
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
    (pp_print_list ~pp_sep:(fun fmt () -> ())
       (fun fmt (l, u) ->
          fprintf fmt "[%a .. %a]" pp_expr l pp_expr u
       )
    ) assigns.content.assign_offset

let pp_assumes fmt (assumes:assumes with_range) =
  fprintf fmt "assumes  : %a;" pp_formula assumes.content

let pp_ensures fmt ensures =
  fprintf fmt "ensures  : %a;" pp_formula ensures.content

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
  | S_warn warn -> pp_warn fmt warn

let pp_leaf_sections fmt secs =
  fprintf fmt "@[<v>";
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@,")
    pp_leaf_section
    fmt secs
  ;
  fprintf fmt "@]"

let pp_case fmt case =
  fprintf fmt "@[<v 2>case \"%s\" {@,%a@]@,}"
    case.case_label
    pp_leaf_sections case.case_body

let pp_section fmt sec =
  match sec with
  | S_leaf leaf -> pp_leaf_section fmt leaf
  | S_case case -> pp_case fmt case

let pp_sections fmt secs =
  fprintf fmt "@[<v>";
  pp_print_list
    ~pp_sep:(fun fmt () -> fprintf fmt "@,")
    pp_section
    fmt secs
  ;
  fprintf fmt "@]"

let pp_stub_func fmt stub = pp_sections fmt stub.stub_func_body

let pp_stub_directive fmt stub = pp_sections fmt stub.stub_directive_body


(** {2 Registration of expressions} *)
(*  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= *)

let () =
  register_expr_with_visitor {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_stub_call(f1,args1), E_stub_call(f2,args2) ->
          Compare.compose [
            (fun () -> compare f1.stub_func_name f2.stub_func_name);
            (fun () -> Compare.list compare_expr args1 args2);
          ]

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

        | E_stub_alloc r1, E_stub_alloc r2 ->
          compare r1 r2

        | E_stub_resource_mem(x1, res1), E_stub_resource_mem(x2, res2) ->
          Compare.compose [
            (fun () -> compare_expr x1 x2);
            (fun () -> compare res1 res2);
          ]

        | E_stub_primed(e1), E_stub_primed(e2) ->
          compare_expr e1 e2

        | _ -> next e1 e2
      );

    visit  = (fun next e ->
        match ekind e with
        | E_stub_call (f,args) ->
          { exprs = args; stmts = [] },
          (function {exprs = args} -> {e with ekind = E_stub_call(f, args)})


        | E_stub_return -> leaf e

        | E_stub_builtin_call(f, arg) ->
          { exprs = [arg]; stmts = [] },
          (function {exprs = [arg]} -> {e with ekind = E_stub_builtin_call(f, arg)} | _ -> assert false)

        | E_stub_quantified(_,_,S_resource _) -> leaf e
        | E_stub_quantified(q,v,S_interval(a,b)) ->
          { exprs = [a;b]; stmts = [] },
          (function { exprs = [a;b] } -> { e with ekind = E_stub_quantified(q, v, S_interval(a,b)) } | _ -> assert false)

        | E_stub_attribute(o, f) ->
          { exprs = [o]; stmts = [] },
          (function { exprs = [o] } -> { e with ekind = E_stub_attribute(o, f) } | _ -> assert false)

        | E_stub_alloc r -> leaf e

        | E_stub_resource_mem(x, res) ->
          { exprs = [x]; stmts = []},
          (function { exprs = [x] } -> { e with ekind = E_stub_resource_mem(x, res) } | _ -> assert false)

        | E_stub_primed(ee) ->
          { exprs = [ee]; stmts = [] },
          (function { exprs = [ee] } -> { e with ekind = E_stub_primed(ee) } | _ -> assert false)

        | _ -> next e
      );

    print   = (fun next fmt e ->
        match ekind e with
        | E_stub_call (s, args) -> fprintf fmt "stub %s(%a)" s.stub_func_name (pp_list pp_expr ", ") args
        | E_stub_return -> pp_print_string fmt "return"
        | E_stub_builtin_call(f, arg) -> fprintf fmt "%a(%a)" pp_builtin f pp_expr arg
        | E_stub_quantified(FORALL, v, _) -> fprintf fmt "∀%a" pp_var v
        | E_stub_quantified(EXISTS, v, _) -> fprintf fmt "∃%a" pp_var v
        | E_stub_attribute(o, f) -> fprintf fmt "%a:%s" pp_expr o f
        | E_stub_alloc r -> fprintf fmt "alloc res(%s)" r
        | E_stub_resource_mem(x, res) -> fprintf fmt "%a ∈ %a" pp_expr x pp_resource res
        | E_stub_primed(ee) -> fprintf fmt "%a'" pp_expr ee
        | _ -> next fmt e
      );
  }


(** {2 Registration of statements} *)
(*  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

let () =
  register_stmt_with_visitor {
    compare = (fun next s1 s2 ->
        match skind s1, skind s2 with
        | S_stub_directive (stub1), S_stub_directive (stub2) ->
          panic "compare for stubs not supported"

        | S_stub_free(e1), S_stub_free(e2) ->
          compare_expr e1 e2

        | S_stub_assigns a1, S_stub_assigns a2 -> compare_assigns a1 a2

        | S_stub_prepare_all_assigns a1, S_stub_prepare_all_assigns a2
        | S_stub_clean_all_assigns a1, S_stub_clean_all_assigns a2 ->
          Compare.list compare_assigns a1 a2

        | S_stub_requires(e1), S_stub_requires(e2) ->
          compare_expr e1 e2

        | _ -> next s1 s2
      );

    visit = (fun next s ->
        match skind s with
        | S_stub_directive _ -> panic "visitor for S_stub_init not supported"

        | S_stub_free e ->
          { exprs = [e]; stmts = [] },
          (function { exprs = [e] } -> { s with skind = S_stub_free e } | _ -> assert false)

        | S_stub_assigns _ -> panic "visitor for S_stub_assigns not supported"

        | S_stub_prepare_all_assigns _ -> panic "visitor for S_stub_assigns not supported"

        | S_stub_clean_all_assigns _ -> panic "visitor for S_stub_assigns not supported"

        | S_stub_requires e ->
          { exprs = [e]; stmts = [] },
          (function { exprs = [e] } -> { s with skind = S_stub_requires e } | _ -> assert false)

        | _ -> next s
      );

    print = (fun next fmt s ->
        match skind s with
        | S_stub_directive (stub) ->
          fprintf fmt "@[<v 2>/*$$$@,%a@]*/"
            pp_sections stub.stub_directive_body

        | S_stub_free e -> fprintf fmt "free(%a);" pp_expr e

        | S_stub_assigns assigns ->
          fprintf fmt "assigns  : %a%a;"
            pp_expr assigns.assign_target
            (pp_print_list ~pp_sep:(fun fmt () -> ())
               (fun fmt (l, u) ->
                  fprintf fmt "[%a .. %a]" pp_expr l pp_expr u
               )
            ) assigns.assign_offset

        | S_stub_prepare_all_assigns _ -> fprintf fmt "prepare assigns;"

        | S_stub_clean_all_assigns _ -> fprintf fmt "clean assigns;"

        | S_stub_requires e ->
          fprintf fmt "requires %a;" pp_expr e

        | _ -> next fmt s
      );
  }
