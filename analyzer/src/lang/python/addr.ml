(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Heap addresses of Python objects. *)

open Framework.Ast
open Framework.Pp
open Framework.Manager
open Universal.Ast
open Ast

type obj_param =
  | List of Universal.Ast.addr
  | Tuple of Universal.Ast.addr
  | Dict of Universal.Ast.addr
  | Range of Universal.Ast.addr
  | Generator of py_fundec
  | AssumedExn of Universal.Ast.addr

type class_address =
  | C_builtin of string
  | C_user of py_clsdec

type function_address =
  | F_builtin of string
  | F_user of py_fundec

type module_address =
  | M_builtin of string

type Universal.Ast.addr_kind +=
  | A_py_class of class_address * addr list
  | A_py_function of function_address
  | A_py_instance of Universal.Ast.addr * obj_param option
  | A_py_method of Universal.Ast.addr * Universal.Ast.addr
  | A_py_module of module_address
  | A_py_unknown

let mk_function_addr func =
  A_py_function (F_user func)

let mk_class_addr cls mro =
  A_py_class (C_user cls, mro)

let mk_instance_addr cls params =
  A_py_instance(cls, params)

let () =
  Universal.Pp.(
    Format.(
      register_pp_addr_kind (fun default fmt ak ->
          match ak with
          | A_py_class(C_user c, _) -> fprintf fmt "class %a" pp_var c.py_cls_var
          | A_py_class(C_builtin c, _) -> fprintf fmt "class %s" c
          | A_py_function(F_user f) -> fprintf fmt "fun %a" pp_var f.py_func_var
          | A_py_function(F_builtin f) -> fprintf fmt "fun %s" f
          | A_py_instance(c, _) -> fprintf fmt "inst of %a" pp_addr c
          | A_py_method(f, obj) -> fprintf fmt "method %a on %a" pp_addr f pp_addr obj
          | A_py_module(M_builtin m) -> fprintf fmt "%s" m
          | A_py_unknown -> fprintf fmt "?"
          | _ -> default fmt ak
        )
    )
  )
