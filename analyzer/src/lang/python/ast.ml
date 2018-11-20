(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python AST *)

open Framework.Essentials
open Universal.Ast

(*==========================================================================*)
                           (** {2 Constants} *)
(*==========================================================================*)

type constant +=
  | C_py_none
  | C_py_not_implemented
  | C_py_imag of float
  | C_py_empty (** empty value of heap objects inheriting from object *)


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

(** Python-specific types *)
type typ +=
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
  | O_py_and (** and *)
  | O_py_or (** or *)
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

 let is_arith_binop_fun = function
    | "int.__add__"
    | "int.__radd__"
    | "int.__and__"
    | "int.__rand__"
    | "int.__floordiv__"
    | "int.__rfloordiv__"
    | "int.__lshift__"
    | "int.__rlshift__"
    | "int.__mod__"
    | "int.__rmod__"
    | "int.__mul__"
    | "int.__rmul__"
    | "int.__or__"
    | "int.__ror__"
    | "int.__pow__"
    | "int.__rpow__"
    | "int.__truediv__"
    | "int.__rtruediv__"
    | "int.__sub__"
    | "int.__rsub__"
    | "int.__xor__"
    | "int.__rxor__" -> true
    | _ -> false

let is_comp_op = function
  | Framework.Ast.O_eq
  | Framework.Ast.O_ne
  | Framework.Ast.O_lt
  | Framework.Ast.O_le
  | Framework.Ast.O_gt
  | Framework.Ast.O_ge -> true
  | _ -> false

let is_compare_op_fun = function
  | "int.__eq__"
    | "int.__ne__"
    | "int.__lt__"
    | "int.__le__"
    | "int.__gt__"
    | "int.__ge__" -> true
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
  py_func_range: range; (** range of the function *)
}

(** A Python class *)
type py_clsdec = {
  py_cls_var : var; (** class object variable *)
  py_cls_body : stmt;
  py_cls_static_attributes: var list; (** list of declared attributes: static variables and methods *)
  py_cls_bases : expr list; (** base classes *)
  py_cls_decors: expr list;
  py_cls_keywords: (string option * expr) list; (** keywords (None id for **kwargs) *)
  py_cls_range : range; (** range of the class *)
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

type Framework.Ast.program +=
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

let mk_py_bool b range =
  mk_constant (C_bool b) ~etyp:T_bool range

let mk_py_true = mk_py_bool true

let mk_py_false = mk_py_bool false

let mk_py_top t range =
  mk_constant (C_top t) ~etyp:t range

let object_of_expr e =
  match ekind e with
  | E_py_object o -> o
  | _ -> assert false

let addr_of_object (obj:py_object) : Universal.Ast.addr =
  fst obj

let value_of_object (obj:py_object) : expr =
  snd obj

let rec is_py_expr e =
  match ekind e with
  | E_py_undefined _
  | E_py_object _
  | E_py_list _
  | E_py_index_subscript _
  | E_py_slice_subscript _
  | E_py_attribute _
  | E_py_dict _
  | E_py_set _
  | E_py_generator_comprehension _
  | E_py_list_comprehension _
  | E_py_set_comprehension _
  | E_py_dict_comprehension _
  | E_py_call _
  | E_py_yield _
  | E_py_if _
  | E_py_tuple _
  | E_py_bytes _
  | E_py_lambda _
  | E_py_multi_compare _
  | E_constant _ -> true
  | E_var _ (*FIXME {vkind = V_orig}*) -> true
  | E_unop(_, e) -> is_py_expr e
  | E_binop(_, e1, e2) -> is_py_expr e1 && is_py_expr e2
  | _ -> false

let mk_py_none range =
  mk_constant ~etyp:T_py_none C_py_none range
