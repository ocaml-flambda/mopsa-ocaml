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

(** Python AST *)

open Mopsa
open Universal.Ast

(*==========================================================================*)
                           (** {2 Constants} *)
(*==========================================================================*)

type constant +=
  | C_py_ellipsis
  | C_py_none
  | C_py_not_implemented
  | C_py_imag of float


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

(** Python-specific types *)
type typ +=
  | T_py_not_implemented
  | T_py_complex
  | T_py_none
  | T_py_bytes


(*==========================================================================*)
                           (** {2 Expressions} *)
(*==========================================================================*)

(** Python objects *)
type py_object = Universal.Ast.addr (** uid + type *) * expr option (** optional value representation *)

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
  | O_py_not


let is_arith_op = function
  | Universal.Ast.O_plus
  | Universal.Ast.O_minus
  | Universal.Ast.O_mult
  | O_py_mat_mult
  | Universal.Ast.O_div
  | O_py_floor_div
  | Universal.Ast.O_bit_invert
  | Universal.Ast.O_mod
  | Universal.Ast.O_pow
  | Universal.Ast.O_bit_lshift
  | Universal.Ast.O_bit_rshift
  | Universal.Ast.O_bit_and
  | Universal.Ast.O_bit_xor
  | Universal.Ast.O_bit_or ->
    true

  | _ -> false

let is_arith_binop_fun cl str =
  let splitted = String.split_on_char '.' str in
  let l = ListExt.nth splitted (ListExt.length splitted - 1) in
  ListExt.hd splitted = cl &&
    match l with
    | "__add__"
    | "__radd__"
    | "__floordiv__"
    | "__rfloordiv__"
    | "__mod__"
    | "__rmod__"
    | "__mul__"
    | "__rmul__"
    | "__pow__"
    | "__rpow__"
    | "__truediv__"
    | "__rtruediv__"
    | "__sub__"
    | "__rsub__" -> true
    | "__and__"
    | "__rand__"
    | "__rshift__"
    | "__rrshift__"
    | "__lshift__"
    | "__rlshift__"
    | "__or__"
    | "__ror__"
    | "__xor__"
    | "__rxor__" -> cl = "int"
    | _ -> false

let is_comp_op = function
  | O_eq
  | O_ne
  | O_lt
  | O_le
  | O_gt
  | O_ge -> true
  | _ -> false

let is_compare_op_fun cl str =
  let splitted = String.split_on_char '.' str in
  let l = ListExt.nth splitted (ListExt.length splitted - 1) in
  ListExt.hd splitted = cl &&
  match l with
  | "__eq__"
    | "__ne__"
    | "__lt__"
    | "__le__"
    | "__gt__"
    | "__ge__" -> true
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
  | E_py_yield_from of expr
  | E_py_if of expr (** test *) * expr (** body *) * expr (** orelse *)
  | E_py_tuple of expr list
  | E_py_bytes of string
  | E_py_lambda of py_lambda
  | E_py_multi_compare of expr (* left *)
                      * operator list (* ops *)
                      * expr list (* comparators *)
  | E_py_annot of expr
  (* checking type annotations using stubs *)
  | E_py_check_annot of expr * expr
  (** low-level hasattribute working at the object level only *)
  | E_py_ll_hasattr of expr (** object *) * expr (** attribute name *)
  (** low-level attribute access working at the object level only *)
  | E_py_ll_getattr of expr (** object *) * expr (** attribute name *)
  (** low-level attribute setter working at the object level only *)
  | E_py_ll_setattr of expr (** object *) * expr (** attribute name *) * expr option (* expression to bind to obj.attr, or None if we want to delete obj.attr (as tp_setattr behaves in cpython) *)


(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)


(** Python function descriptor *)
type py_fundec = {
  py_func_var: var; (** function object variable *)
  py_func_parameters: var list; (** list of parameters variables *)
  py_func_defaults: expr option list; (** list of default parameters values *)
  py_func_vararg: var option; (* variable argument arg (usually *args), if any *)
  py_func_kwonly_args: var list; (* list of keyword-only arguments *)
  py_func_kwonly_defaults: expr option list; (* default values associated to keyword-only arguments *)
  py_func_kwarg: var option; (* keyword-based variable argument (usually **kwargs) if any *)
  py_func_locals: var list; (** list of local variables *)
  py_func_body: stmt; (** function body *)
  py_func_is_generator: bool; (** is the function a generator? *)
  py_func_decors: expr list;
  py_func_types_in: expr option list;
  py_func_type_out: expr option;
  py_func_range: range; (** range of the function *)
  py_func_ret_var: var
}

type py_func_sig =
  {
    py_funcs_parameters: var list;
    py_funcs_defaults: bool list; (* true iff argument has default *)
    py_funcs_exceptions: expr list;
    py_funcs_types_in: expr option list;
    py_funcs_type_out: expr option;
  }


type py_func_annot = {
  py_funca_var: var;
  py_funca_decors: expr list;
  py_funca_range: range;
  py_funca_ret_var: var;
  py_funca_sig: py_func_sig list;
}

let pp_py_func_sig (fmt: Format.formatter) (sign: py_func_sig) =
  (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") (fun fmt (p, a) ->
       Format.fprintf fmt "%a: %a" pp_var p (Option.print pp_expr) a))
    fmt (List.combine sign.py_funcs_parameters sign.py_funcs_types_in)

let pp_py_func_annot (fmt:Format.formatter) (a:py_func_annot) =
  List.iter (fun sign ->
      Format.fprintf fmt "%a%a(%a) -> %a: ...@\n"
          (fun fmt _ -> if a.py_funca_decors = [] then Format.fprintf fmt ""
           else Format.fprintf fmt "@%a@\n" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") pp_expr) a.py_funca_decors) ()
          pp_var a.py_funca_var
          pp_py_func_sig sign
          (Option.print pp_expr) sign.py_funcs_type_out
    ) a.py_funca_sig

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

type py_cls_annot = {
  py_cls_a_var : var;
  py_cls_a_body : stmt;
  py_cls_a_bases : expr list;
  py_cls_a_abases : expr list; (* bases from the typing module, hopefully *)
  py_cls_a_static_attributes: var list;
  py_cls_a_range : range;
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

  (** if condition *)
  | S_py_if of expr (*t test *) * stmt (* then *) * stmt (* else *)

  (** while loops. *)
  | S_py_while of expr (* test *) * stmt (* body *) * stmt (* orelse *)

  (** assign a expression to a list of lvals *)
  | S_py_multi_assign of expr list * expr

  (** increment assignments *)
  | S_py_aug_assign of expr * operator * expr

  (** type annotations for variables *)
  | S_py_annot of expr * expr

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

type prog_kind +=
  | Py_program of
      string (** name *) *
      var list (** global variables *) *
      stmt (** body *)


(*==========================================================================*)
                           (** {2 Utility functions} *)
(*==========================================================================*)

let mk_py_in ?(strict = false) ?(left_strict = false) ?(right_strict = false) v e1 e2 erange =
  match strict, left_strict, right_strict with
  | true, _, _
  | false, true, true ->
    mk_binop
      (mk_binop e1 O_lt v erange)
      O_py_and
      (mk_binop v O_lt e2 erange)
      erange

  | false, true, false ->
    mk_binop
      (mk_binop e1 O_lt v erange)
      O_py_and
      (mk_binop v O_le e2 erange)
      erange

  | false, false, true ->
    mk_binop
      (mk_binop e1 O_le v erange)
      O_py_and
      (mk_binop v O_lt e2 erange)
      erange

  | false, false, false ->
    mk_binop
      (mk_binop e1 O_le v erange)
      O_py_and
      (mk_binop v O_le e2 erange)
      erange

let mk_py_not exp range =
  mk_unop O_py_not exp range

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

let mk_py_kall func args kwargs range =
  (* call with kwargs *)
  mk_expr (E_py_call (func, args, kwargs)) range

let mk_py_attr obj attr ?(etyp=T_any) range =
  mk_expr (E_py_attribute (obj, attr)) ~etyp range

let mk_py_object (addr, e) range =
  mk_expr (E_py_object (addr, e)) range

let mk_py_object_attr obj attr ?(etyp=T_any) range =
  mk_py_attr (mk_py_object obj range) attr ~etyp range

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

let value_of_object (obj:py_object) : expr option =
  snd obj

let mk_py_none range =
  mk_constant ~etyp:T_py_none C_py_none range
