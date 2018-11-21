(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   AST - A more abstract representation than the original Python CST.

   Contains some additional static information, such as unique variables IDs and
   the list of local variables of functions.
*)

(** A variable with a unique identifier *)
type var = {
  name: string; (** original name in the source code *)
  uid: int; (** a unique identifier at the scope level *)
}

type program = {
  prog_body : stmt;
  prog_globals : var list;
}

(** Statements *)
and stmt = {
  skind : stmt_kind;
  srange : Location.range;
}

and binop =
  | O_arithmetic of Cst.binop
  | O_comparison of Cst.cmpop
  | O_bool of Cst.boolop

and stmt_kind =
  | S_assign
    of expr (** target *) *
       expr (** value *)

  | S_expression
    of expr (** expression statements *)

  | S_while
    of expr (** test *) *
       stmt (** body *) *
       stmt option (** else *)

  | S_break

  | S_continue

  | S_block
    of stmt list

  | S_aug_assign (** such as x += e *)
    of expr (** taregt *) *
       binop (** operator *) *
       expr (** value *)

  | S_if
    of expr (** test *) *
       stmt (** then branch *) *
       stmt option (** optional else branch *)

  | S_function (** function definition *)
    of func

  | S_class (** class definition *)
    of cls

  | S_for (** for loops *)
    of expr (** target *) *
       expr (** iterator *) *
       stmt (** body *) *
       stmt option (** else *)

  | S_return (** return *)
    of expr (** return expression. Empty returns are equivalent to return None *)

  | S_raise of expr option (** exn *) * expr option (** cause *)

  (** try/except statement *)
  | S_try
    of stmt (** body *) *
       except list (** except handlers *)  *
       stmt option (** else body *) *
       stmt option (** final body *)

  | S_import of string (** module *) * var option (** asname *) * var (** root module var *)
  | S_import_from of string (** module *) * string (** name *) * var (** root module var *) * var (** module var *)
  (** Import statements *)

  | S_delete of expr

  | S_assert of expr * expr option

  | S_with of expr (** context *) * expr option (** as *) * stmt (** body *)

  | S_pass

(** Exception handler *)
and except =
  expr option (** exception type *) *
  var option (** as name *) *
  stmt (** body *)

(** Function declaration *)
and func = {
  func_var: var; (** function object variable *)
  func_parameters: var list; (** list of parameters variables *)
  func_defaults: expr option list; (** list of default parameters values *)
  func_locals: var list; (** list of local variables *)
  func_globals: var list; (** list of variables declared as global *)
  func_nonlocals: var list; (** list of variables declared as nonlocal *)
  func_body: stmt; (** function body *)
  func_is_generator: bool; (** is the function a generator? *)
  func_decors: expr list;
  func_return: expr option;
  func_range: Location.range;
}

(** Class definition *)
and cls = {
  cls_var: var; (** class object variable *)
  cls_body: stmt; (** class body *)
  cls_static_attributes: var list;
  cls_bases: expr list; (** inheritance base classes *)
  cls_decors: expr list;
  cls_keywords: (string option * expr) list; (** keywords (None id for **kwargs) *)
  cls_range: Location.range;
}


and lambda = {
  lambda_body: expr;
  lambda_parameters: var list; (** list of parameters variables *)
  lambda_defaults: expr option list; (** list of default parameters values *)
}

(** Expressions *)
and expr = {
  ekind : expr_kind;
  erange : Location.range;
}

and expr_kind =
    | E_true

    | E_false

    | E_none

    | E_notimplemented

    | E_num of Cst.number

    | E_str of string

    | E_bytes of string

    (** attribute access *)
    | E_attr of
        expr (** object *) *
        string (** attribute name *)

    | E_id of var

    (** function call *)
    | E_call of
        expr (** function expression *)*
        expr list (** arguments *) *
        (string option * expr) list (** keywords (None id for **kwargs) *)

    | E_list of expr list (** list of elements *)

    (** Index-based subscript access *)
    | E_index_subscript of
        expr (** object *) * expr (** index *)

    (** Slice-based subscript access *)
    | E_slice_subscript of
        expr (** object *) *
        expr (** Lower bound. None if not specified *) *
        expr (** Uper bound. None if not specified *) *
        expr (** Step. None if not specified *)

    | E_tuple of expr list

    | E_set of expr list

    | E_dict of expr list (** keys *) * expr list (** values *)

    (** Generator comprehension *)
    | E_generator_comp of
        expr (** generator expression *) *
        comprehension list (** list of comprehensions *)

    (** List comprehension *)
    | E_list_comp of
        expr (** element expression *) *
        comprehension list (** list of comprehensions *)

    (** Set comprehension *)
    | E_set_comp of
        expr (** element expression *) *
        comprehension list (** list of comprehensions *)

    (** Dictionary comprehension *)
    | E_dict_comp of
        expr (** key expression *) *
        expr (** value expression *) *
        comprehension list (** list of comprehensions *)

    (** If expressions *)
    | E_if of
        expr (* test *) *
        expr (* body *) *
        expr (* else *)


    (** Yield expression *)
    | E_yield of expr

    (** Binary operator expressions *)
    | E_binop of
        expr (** left operand *) * binop * expr (** right operand *)

    | E_multi_compare of expr (* left *)
                         * binop list (* ops *)
                         * expr list (* comparators *)


    (** Unary operato expressions *)
    | E_unop of Cst.unop * expr

    | E_lambda of lambda

(** Comprehensions *)
and comprehension =
  expr (** target *) *
  expr (** iterator *) *
  expr list (** list of conditions *)


module VarSet = Set.Make(struct type t = var let compare = compare end)
module VarMap = MapExt.Make(struct type t = var let compare = compare end)
module VarSetMap = MapExt.Make(struct type t = VarSet.t let compare = VarSet.compare end)
