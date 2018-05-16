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
  | C_py_none
  | C_py_undefined
  | C_py_not_implemented
  | C_py_imag of float


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

(** Python-specific types *)
type typ +=
  | T_py_undefined
  | T_py_not_implemented
  | T_py_complex
  | T_py_none


(*==========================================================================*)
                           (** {2 Expressions} *)
(*==========================================================================*)

type operator +=
  | O_py_and (** and *)
  | O_py_or (** or *)
  | O_py_floor_div (** // *)
  | O_py_is (** is *)
  | O_py_is_not (** is not *)
  | O_py_in (** in *)
  | O_py_not_in (** not in *)
  | O_py_mat_mult (** @ *)

(** Lambda functions. *)
type py_lambda = {
  py_lambda_body: expr; (** Body. *)
  py_lambda_parameters: var list; (** list of parameters variables *)
  py_lambda_defaults: expr option list; (** list of default parameters values *)
}

type expr_kind +=
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

let mk_py_attr obj attr range =
  mk_expr (E_py_attribute (obj, attr)) range

let mk_py_addr_attr addr attr range =
  mk_py_attr (Universal.Ast.mk_addr addr (tag_range range "addr")) attr range

let mk_py_none range =
  mk_constant ~etyp:T_py_none C_py_none range

let mk_py_not_implemented range =
  mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range
