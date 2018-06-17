(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python AST *)

open Framework.Ast


(*==========================================================================*)
                           (** {2 Constants} *)
(*==========================================================================*)

type constant +=
  | C_py_true
  | C_py_false
  | C_py_none
  | C_py_not_implemented
  | C_py_imag of float
  | C_py_empty (** empty value of heap objects inheriting from object *)


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

(** Python-specific types *)
type typ +=
  | T_py_bool
  | T_py_not_implemented
  | T_py_complex
  | T_py_none
  | T_py_empty


(*==========================================================================*)
                           (** {2 Expressions} *)
(*==========================================================================*)

(** Python objects *)
type py_object = Universal.Ast.addr (** uid + type *) * expr (** value representation *)

let compare_py_object (obj1: py_object) (obj2: py_object) : int =
  let addr1 = fst obj1 and addr2 = fst obj2 in
  Universal.Ast.compare_addr addr1 addr2

type operator +=
  | O_py_floor_div (** // *)
  | O_py_is (** is *)
  | O_py_is_not (** is not *)
  | O_py_in (** in *)
  | O_py_not_in (** not in *)
  | O_py_mat_mult (** @ *)


let is_arith_op = function
  | Universal.Ast.O_plus
  | Universal.Ast.O_minus
  | Universal.Ast.O_mult
  | O_py_mat_mult
  | Universal.Ast.O_div
  | O_py_floor_div
  | Universal.Ast.O_mod
  | Universal.Ast.O_pow
  | Universal.Ast.O_bit_lshift
  | Universal.Ast.O_bit_rshift
  | Universal.Ast.O_bit_and
  | Universal.Ast.O_bit_xor
  | Universal.Ast.O_bit_or ->
    true

  | _ -> false

let is_comp_op = function
  | Universal.Ast.O_eq
  | Universal.Ast.O_ne
  | Universal.Ast.O_lt
  | Universal.Ast.O_le
  | Universal.Ast.O_gt
  | Universal.Ast.O_ge -> true
  | _ -> false


(** Lambda functions. *)
type py_lambda = {
  py_lambda_body: expr; (** Body. *)
  py_lambda_parameters: var list; (** list of parameters variables *)
  py_lambda_defaults: expr option list; (** list of default parameters values *)
}

type expr_kind +=
  | E_py_undefined of bool (* is it global? *)
  | E_py_object of py_object
  | E_py_list of expr list
  | E_py_index_subscript of expr (** object *) * expr (** index *)
  | E_py_slice_subscript of expr (** object *) * expr (** start *) * expr (** end *) * expr (** step *)
  | E_py_attribute of expr (** object *) * string (** attribute name *)
  | E_py_dict of expr list (** keys *) * expr list (** values *)
  | E_py_set of expr list
  | E_py_generator_comprehension of
      expr (** generator expression *) *
      (
        expr (** target *) *
        expr (** iterator *) *
        expr list (** list of conditions *)
      ) list (** list of comprehensions *)

  | E_py_list_comprehension of
      expr (** value expression *) *
      (
        expr (** target *) *
        expr (** iterator *) *
        expr list (** list of conditions *)
      ) list (** list of comprehensions *)

  | E_py_set_comprehension of
      expr (** value expression *) *
      (
        expr (** target *) *
        expr (** iterator *) *
        expr list (** list of conditions *)
      ) list (** list of comprehensions *)

  | E_py_dict_comprehension of
      expr (** key expression *) *
      expr (** value expression *) *
      (
        expr (** target *) *
        expr (** iterator *) *
        expr list (** list of conditions *)
      ) list (** list of comprehensions *)

  | E_py_call of expr (** function *) *
              expr list (** arguments *) *
              (string option * expr) list (** keywords (None id for **kwargs) *)
  | E_py_yield of expr
  | E_py_if of expr (** test *) * expr (** body *) * expr (** orelse *)
  | E_py_tuple of expr list
  | E_py_bytes of string
  | E_py_lambda of py_lambda
  | E_py_multi_compare of expr (* left *)
                      * operator list (* ops *)
                      * expr list (* comparators *)


(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)


(** Python function descriptor *)
type py_fundec = {
  py_func_var: var; (** function object variable *)
  py_func_parameters: var list; (** list of parameters variables *)
  py_func_defaults: expr option list; (** list of default parameters values *)
  py_func_locals: var list; (** list of local variables *)
  py_func_body: stmt; (** function body *)
  py_func_is_generator: bool; (** is the function a generator? *)
  py_func_decors: expr list;
}

(** A Python class *)
type py_clsdec = {
  py_cls_var : var; (** class object variable *)
  py_cls_body : stmt;
  py_cls_static_attributes: var list; (** list of declared attributes: static variables and methods *)
  py_cls_bases : expr list; (** base classes *)
  py_cls_decors: expr list;
  py_cls_keywords: (string option * expr) list (** keywords (None id for **kwargs) *)
}



(** Exception handler *)
type py_excpt = {
  py_excpt_type : expr option; (** exception class. None is used for the default except *)
  py_excpt_name : var option; (** optional name of exception instance *)
  py_excpt_body : stmt; (** body of the except handler *)
}


(** Statements *)
type stmt_kind +=
  (** class definition *)
  | S_py_class of py_clsdec

  (** function definition *)
  | S_py_function of py_fundec

  (** try/except statements *)
  | S_py_try
    of stmt (** body *) *
       py_excpt list (** exception handlers *) *
       stmt (** else body *) *
       stmt (** final body *)

  (** exception instance *)
  | S_py_raise of expr option

  (** while loops. *)
  | S_py_while of expr (* test *) * stmt (* body *) * stmt (* orelse *)

  (** assign a expression to a list of lvals *)
  | S_py_multi_assign of expr list * expr

  (** increment assignments *)
  | S_py_aug_assign of expr * operator * expr

  (** for loops *)
  | S_py_for of expr (** target *) *
             expr (** iterator *) *
             stmt (** body *) *
             stmt (** else *)

  (** package import *)
  | S_py_import of string (** module *) *
                   var option (** asname *) *
                   var (** root module *)


  | S_py_import_from of string (** module *) *
                        string (** name *) *
                        var (** root module *) *
                        var (** module var *)

  | S_py_delete of expr

  | S_py_assert of expr (** test *) * expr option (** message *)

  | S_py_with of expr (** context *) *
              expr option (** as *) *
              stmt (** body *)


(*==========================================================================*)
                           (** {2 Programs} *)
(*==========================================================================*)

type Framework.Ast.program_kind +=
  | Py_program of
      var list (** global variables *) *
      stmt (** body *)


(*==========================================================================*)
                           (** {2 Utility functions} *)
(*==========================================================================*)


let mk_except typ name body =
  {
    py_excpt_type = typ;
    py_excpt_name = name;
    py_excpt_body = body;
  }


let mk_try body except orelse finally range =
  mk_stmt
    (S_py_try (
      body,
      except,
      orelse,
      finally
    ))
    range

let mk_raise exc range =
  mk_stmt (S_py_raise (Some exc)) range

let mk_py_call func args range =
  mk_expr (E_py_call (func, args, [])) range

let mk_py_attr obj attr ?(etyp=T_any) range =
  mk_expr (E_py_attribute (obj, attr)) ~etyp range

let mk_py_object (addr, e) range =
  mk_expr (E_py_object (addr, e)) range

let mk_py_object_attr obj attr ?(etyp=T_any) range =
  mk_py_attr (mk_py_object obj range) attr ~etyp range

let mk_py_empty range =
  mk_constant C_py_empty ~etyp:T_py_empty range

let object_of_expr e =
  match ekind e with
  | E_py_object o -> o
  | _ -> assert false

let addr_of_object (obj:py_object) : Universal.Ast.addr =
  fst obj

let value_of_object (obj:py_object) : expr =
  snd obj


(** Pretty printer of the Python AST extension *)

open Format

let pp_except fmt e =
  fprintf fmt "except %a%a:@\n@[<h 2>  %a@]"
    (fun fmt e -> match e with
       | None -> ()
       | Some e -> pp_expr fmt e
    ) e.py_excpt_type
    (fun fmt v -> match v with
       | None -> ()
       | Some v -> fprintf fmt " as %a" pp_var v
    ) e.py_excpt_name
    pp_stmt e.py_excpt_body

let pp_py_object fmt (obj: py_object) =
  match obj with
  | (addr, {ekind = E_constant (C_py_empty)}) -> fprintf fmt "%a" Universal.Pp.pp_addr addr
  | (addr, e) -> fprintf fmt "⟪%a :: %a⟫" Universal.Pp.pp_addr addr pp_expr e

let () =
  register_pp_program (fun default fmt prog ->
      match prog.prog_kind with
      | Py_program(globals, body) -> pp_stmt fmt body
      | _ -> default fmt prog
    );
  register_pp_typ (fun default fmt typ ->
    match typ with
    | T_py_not_implemented -> pp_print_string fmt "notimplemented"
    | T_py_none -> pp_print_string fmt "none"
    | T_py_complex -> pp_print_string fmt "complex"
    | T_py_empty -> pp_print_string fmt "empty"
    | _ -> default fmt typ
    );
  register_pp_constant (fun default fmt -> function
      | C_py_none -> pp_print_string fmt "None"
      | C_py_not_implemented -> pp_print_string fmt "NotImplemented"
      | C_py_imag j -> fprintf fmt "%aj" pp_print_float j
      | C_py_empty -> pp_print_string fmt "empty"
      | c -> default fmt c
    );
  register_pp_operator (fun default fmt -> function
      | O_py_floor_div -> pp_print_string fmt "//"
      | O_py_is -> pp_print_string fmt "is"
      | O_py_is_not -> pp_print_string fmt "is not"
      | O_py_in -> pp_print_string fmt "in"
      | O_py_not_in -> pp_print_string fmt "not in"
      | O_py_mat_mult -> pp_print_string fmt "@"
      | op -> default fmt op
    );
  register_pp_expr (fun default fmt exp ->
      match ekind exp with
      | E_py_undefined true -> fprintf fmt "global undef"
      | E_py_undefined false -> fprintf fmt "local undef"
      | E_py_object obj -> pp_py_object fmt obj
      | E_py_attribute(obj, attr) ->
        fprintf fmt "pyattr %a.%s" pp_expr obj attr
      | E_py_list(elts) ->
        fprintf fmt "[%a]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) elts
      | E_py_tuple(elts) ->
        fprintf fmt "(%a)"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) elts
      | E_py_set(elts) ->
        fprintf fmt "{%a}"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) elts
      | E_py_dict(keys, values) ->
        fprintf fmt "{%a}"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
             (fun fmt (k, v) -> fprintf fmt "%a: %a" pp_expr k pp_expr v)
          ) (List.combine keys values)

      | E_py_index_subscript(obj, index) ->
        fprintf fmt "%a[%a]" pp_expr obj pp_expr index
      | E_py_slice_subscript(obj, a, b, s) ->
        fprintf fmt "%a[%a : %a : %a]" pp_expr obj pp_expr a pp_expr b pp_expr s
      | E_py_call(f, args, keywords) ->
        fprintf fmt "%a(%a%a%a)"
          pp_expr f
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) args
          (fun fmt -> function [] -> () | _ -> pp_print_string fmt ", ") keywords
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
             (fun fmt -> function
                | (None, kwd) -> fprintf fmt "**%a" pp_expr kwd
                | (Some k, v) -> fprintf fmt "%s = %a" k pp_expr v
             )
          ) keywords
      | E_py_yield(e) ->
        fprintf fmt "yield %a" pp_expr e
      | E_py_if (test, body, orelse) ->
        fprintf fmt
          "%a if %a else %a"
          pp_expr body
          pp_expr test
          pp_expr orelse
      | E_py_list_comprehension(e, comprhs) ->
        fprintf fmt
          "[%a %a]"
          pp_expr e
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ") (fun fmt (target, iter, conds) ->
               fprintf fmt
                 "for %a in %a%a"
                 pp_expr target
                 pp_expr iter
                 (pp_print_list (fun fmt cond -> fprintf fmt " if %a" pp_expr cond)) conds
             )
          ) comprhs
      | E_py_set_comprehension(e, comprhs) ->
        fprintf fmt
          "{%a %a}"
          pp_expr e
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ") (fun fmt (target, iter, conds) ->
               fprintf fmt
                 "for %a in %a%a"
                 pp_expr target
                 pp_expr iter
                 (pp_print_list (fun fmt cond -> fprintf fmt " if %a" pp_expr cond)) conds
             )
          ) comprhs
      | E_py_generator_comprehension(e, comprhs) ->
        fprintf fmt
          "(%a %a)"
          pp_expr e
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ") (fun fmt (target, iter, conds) ->
               fprintf fmt
                 "for %a in %a%a"
                 pp_expr target
                 pp_expr iter
                 (pp_print_list (fun fmt cond -> fprintf fmt " if %a" pp_expr cond)) conds
             )
          ) comprhs
      | E_py_dict_comprehension(k, v, comprhs) ->
        fprintf fmt
          "{%a: %a %a}"
          pp_expr k
          pp_expr v
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ") (fun fmt (target, iter, conds) ->
               fprintf fmt
                 "for %a in %a%a"
                 pp_expr target
                 pp_expr iter
                 (pp_print_list (fun fmt cond -> fprintf fmt " if %a" pp_expr cond)) conds
             )
          ) comprhs

      | E_py_lambda l ->
        fprintf fmt "lambda %a: %a"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_var) l.py_lambda_parameters
          pp_expr l.py_lambda_body
      | E_py_multi_compare(left, ops, rights) ->
        let l = List.combine ops rights in
        fprintf fmt "%a %a"
          pp_expr left
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ")
             (fun fmt (op, right) -> fprintf fmt "%a %a" pp_operator op pp_expr right)
          ) l

      | E_py_bytes(s) ->
        fprintf fmt "b\"%s\"" s

      | _ -> default fmt exp
    );

  register_pp_stmt (fun default fmt stmt ->
      match skind stmt with
      | S_py_class(cls) ->
        fprintf fmt "class %a:@\n@[<h 2>  %a@]" pp_var cls.py_cls_var pp_stmt cls.py_cls_body

      | S_py_function(func) ->
        fprintf fmt "%a@\ndef %a(%a):@\n@[<h 2>  %a@]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") (fun fmt d -> fprintf fmt "@@%a" pp_expr d)) func.py_func_decors
          pp_var func.py_func_var
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_var) func.py_func_parameters
          pp_stmt func.py_func_body

      | S_py_try(body, excepts, orelse, final) ->
        fprintf fmt "try:@\n@[<h 2>  %a@]@\n%a@\nelse:@[<h 2>  %a@]@\nfinal:@[<h 2>  %a@]"
          pp_stmt body
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") pp_except) excepts
          pp_stmt orelse
          pp_stmt final

      | S_py_raise e ->
        fprintf fmt "raise %a" (fun fmt -> function None -> () | Some e -> pp_expr fmt e) e

      | S_py_while(test, body, orelse) ->
        fprintf fmt "while %a:@\n@[<h 2>  %a@]@\nelse:@\n@[<h 2>  %a@]"
          pp_expr test
          pp_stmt body
          pp_stmt orelse

      | S_py_multi_assign(targets, e) ->
        fprintf fmt "%a %a"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") (fun fmt x -> fprintf fmt "%a =" pp_expr x)) targets
          pp_expr e

      | S_py_aug_assign(x, op, e) ->
        fprintf fmt "%a %a= %a"
          pp_expr x
          pp_operator op
          pp_expr e

      | S_py_for(target, iter, body, orelse) ->
        fprintf fmt "for %a in %a:@\n@[<h 2>  %a@]@\nelse:@\n@[<h 2>  %a@]"
          pp_expr target
          pp_expr iter
          pp_stmt body
          pp_stmt orelse

      | S_py_import(mdl, asname, vroot) ->
        fprintf fmt "import %s%a"
          mdl
          (fun fmt -> function None -> () | Some name -> fprintf fmt " as %a" pp_var name) asname


      | S_py_import_from(mdl, name, vroot, asname) ->
        fprintf fmt "from %s import %s as %a"
          mdl
          name
          pp_var asname

      | S_py_delete e -> fprintf fmt "del %a" pp_expr e

      | S_py_assert(e, None) -> fprintf fmt "assert %a" pp_expr e
      | S_py_assert(e, Some msg) -> fprintf fmt "assert %a, %a" pp_expr e pp_expr msg

      | S_py_with(ctx, asname, body) ->
        fprintf fmt "with %a%a:@\n@[<h 2>  %a@]"
          pp_expr ctx
          (fun fmt -> function None -> () | Some name -> fprintf fmt " as %a" pp_expr name) asname
          pp_stmt body

      | _ -> default fmt stmt
    );
  ()
