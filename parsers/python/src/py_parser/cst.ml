(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** 
   The definition of the ast can be found at:
   https://docs.python.org/3/library/ast.html#abstract-grammar
*)

open Location

type identifier = string

and stmt = {
  skind : stmt_kind;
  srange : range;
}

and stmt_kind =
    | FunctionDef of identifier (* name *)
            * arguments (* args *)
            * stmt list (* body *)
            * expr list (* decorator list *)
            * expr option (* returns *)
    | AsyncFunctionDef of identifier (* name *)
            * arguments (* args *)
            * stmt list (* body *)
            * expr list (* decorator list *)
            * expr option (* returns *)
    | ClassDef of identifier (* name *)
            * expr list (* bases *)
            * keyword list (* keywords *)
            * stmt list (* body *)
            * expr list (* decorator list *)
    | Return of expr option (* value *)
    | Delete of expr list (* targets *)
    | Assign of expr list (* targets *)
            * expr (* value *)
    | AugAssign of expr (* target *)
            * binop (* op *)
            * expr (* value *)
    | AnnAssign of expr (* target *)
            * expr (* annotation *)
            * expr option (* value *)
    | For of expr (* target *)
            * expr (* iter *)
            * stmt list (* body *)
            * stmt list (* else *)
    | AsyncFor of expr (* target *)
            * expr (* iter *)
            * stmt list (* body *)
            * stmt list (* else *)
    | While of expr (* target *)
            * stmt list (* body *)
            * stmt list (* else *)
    | If of expr (* test *)
            * stmt list (* body *)
            * stmt list (* else *)
    | With of withitem list (* items *)
            * stmt list (* body *)
    | AsyncWith of withitem list (* items *)
            * stmt list (* body *)
    | Raise of expr option (* exc *)
            * expr option (* cause *)
    | Try of stmt list (* body *)
            * excepthandler list (* handlers *)
            * stmt list (* else *)
            * stmt list (* finalbody *)
    | Assert of expr (* test *)
            * expr option (* msg *)
    | Import of alias list (* names *)
    | ImportFrom of identifier option (* module *)
            * alias list (* names *)
            * int option (* level *)
    | Global of identifier list (* names *)
    | Nonlocal of identifier list (* names *)
    | Expr of expr (* value *)
    | Pass
    | Break
    | Continue

and expr = {
  ekind : expr_kind;
  erange : range;
}

and expr_kind =
    | BoolOp of boolop (* operator *)
            * expr list (* values *)
    | BinOp of expr (* left *)
            * binop (* op *)
            * expr (* right *)
    | UnaryOp of unop (* op *)
            * expr (* operand *)
    | Lambda of arguments (* args *)
            * expr (* body *)
    | IfExp of expr (* test *)
            * expr (* body *)
            * expr (* else *)
    | Dict of expr list (* keys *)
            * expr list (* values *)
    | Set of expr list (* elts *)
    | ListComp of expr (* elt *)
            * comprehension list (* generators *)
    | SetComp of expr (* elt *)
            * comprehension list (* generators *)
    | DictComp of expr (* key *)
            * expr (* value *)
            * comprehension list (* generators *)
    | GeneratorExp of expr (* elt *)
            * comprehension list (* generators *)
    (* The grammar constraints where yield expressions can occur *)
    | Await of expr (* value *)
    | Yield of expr option (* value *)
    | YieldFrom of expr (* value *)
    | Compare of expr (* left *)
            * cmpop list (* ops *)
            * expr list (* comparators *)
    | Call of expr (* func *)
            * expr list (* args *)
            * keyword list (* keywords *)
    | Num of number (* n *) (* number as PyObject *)
    | Str of string (* s *)
    | FormattedValue of expr (* value *)
            * int option (* conversion *)
            * expr option (* format_spec *)
    | JoinedStr of expr list (* values *)
    | Bytes of string (* s *)
    | NameConstant of singleton (* value *)
    | Ellipsis
(*    | Constant of constant (* value *) *)
    (* The following expression can appear in assignment context *)
    | Attribute of expr (* value *)
            * identifier (* attr *)
            * expr_context (* ctx *)
    | Subscript of expr (* value *)
            * slice (* slice *)
            * expr_context (* ctx *)
    | Starred of expr (* value *)
            * expr_context (* ctx *)
    | Name of identifier (* id *)
            * expr_context (* ctx *)
    | List of expr list (* elts *)
            * expr_context (* ctx *)
    | Tuple of expr list (* elts *)
            * expr_context (* ctx *)
    | Null (* should raise an error if accessed *)

and expr_context =
    | Load
    | Store
    | Del
    | AugLoad
    | AugStore
    | Param

and slice =
    | Slice of expr option (* lower *)
            * expr option (* upper *)
            * expr option (* step *)
    | ExtSlice of slice list (* dims *)
    | Index of expr (* value *)

and boolop =
    | And
    | Or

and binop =
    | Add
    | Sub
    | Mult
    | MatMult
    | Div
    | Mod
    | Pow
    | LShift
    | RShift
    | BitOr
    | BitXor
    | BitAnd
    | FloorDiv

and unop =
    | Invert
    | Not
    | UAdd
    | USub

and cmpop =
    | Eq
    | NotEq
    | Lt
    | LtE
    | Gt
    | GtE
    | Is
    | IsNot
    | In
    | NotIn

and comprehension = expr (* target *)
        * expr (* iter *)
        * expr list (* ifs *)
        * bool (* is_async *)

and excepthandler =
    | ExceptHandler of expr option (* type *)
            * identifier option (* name *)
            * stmt list (* body *)

and arguments = arg list (* args *)
        * arg option (* vararg *)
        * arg list (* kwonlyargs *)
        * expr list (* kw_defaults *)
        * arg option (* kwarg *)
        * expr list (* defaults *)

and arg = identifier (* arg *)
        * expr option (* annotation *)

(* keyword arguments supplied to call (NULL identifier for **kwargs) *)
and keyword = identifier option (* arg *)
        * expr (* value *)

(* import name with optional 'as' alias *)
and alias = identifier (* name *) * identifier option (* asname *)

and withitem = expr (* context_expr *) * expr option (* optional_vars *)

and number =
    | Int of Z.t
    | Float of float
    | Imag of string

and singleton =
    | True
    | False
    | SNone
    | SNotImplemented
