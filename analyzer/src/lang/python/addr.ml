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
open Framework.Eval
open Universal.Ast
open Ast


let debug fmt = Debug.debug ~channel:"python.addr" fmt


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

let mk_method_addr f obj =
  A_py_method (f, obj)


let eval_alloc_instance (man: ('a, 't) manager) ctx cls params range flow : (addr, 'a) evals option =
  let exp = mk_alloc_addr (mk_instance_addr cls params) range range in
  man.eval ctx exp flow |>
  eval_compose (fun exp flow ->
      match ekind exp with
      | E_addr addr -> oeval_singleton (Some addr, flow, [])
      | _ -> Framework.Exceptions.panic "eval_alloc_instance: allocation returned a non-address express %a" Framework.Pp.pp_expr exp
    )

(* Builtins *)
let modules : addr list ref = ref []
let classes : addr list ref = ref []
let functions : addr list ref = ref []
let exceptions : addr list ref = ref []

let all () = !modules @ !classes @ !functions @ !exceptions


let rec builtin_addr_to_name addr =
  match addr.addr_kind with
  | A_py_class(C_builtin(name), _) | A_py_function(F_builtin(name)) | A_py_module(M_builtin(name)) ->
    name

  | A_py_method(cls, func) -> (builtin_addr_to_name cls) ^ "." ^ (builtin_addr_to_name func)

  | _ -> assert false


let mk_unname base name =
  match base with
  | None -> name
  | Some base -> base ^ "." ^ name


let from_string name =
  debug "searching for builtin %s" name;
  List.find (fun addr -> debug "addr = %a" Universal.Pp.pp_addr addr; name = builtin_addr_to_name addr) (all ())


let from_attribute obj attr =
  let base = from_string obj in
  match base.addr_kind with
  | A_py_module(M_builtin name)
  | A_py_class(C_builtin name, _) ->
    from_string (mk_unname (Some name) attr)

  | _ -> assert false

let is_builtin name = List.exists (fun addr -> name = builtin_addr_to_name addr) (all ())

let is_builtin_module name = List.exists (fun addr -> name = builtin_addr_to_name addr) !modules

let is_builtin_attribute name attr =
  let base = from_string name in
  match base.addr_kind with
  | A_py_module(M_builtin name)
  | A_py_class(C_builtin name, _) ->
    is_builtin (mk_unname (Some name) attr)

  | _ -> false

let split_class_dot_attribute x =
  let l = String.split_on_char '.' x in
  match l with
  | [cls; attr] -> Some (cls, attr)
  | [modul; cls; attr] -> Some (modul ^ "." ^ cls, attr)
  | _ -> None

let find_type_class_name = function
  | T_int -> "int"
  | T_float -> "float"
  | T_bool -> "bool"
  | T_string -> "str"
  | _ -> assert false

let find_type_function t f =
  let cls = find_type_class_name t in
  from_attribute cls f

let classof e =
  match etyp e with
  | T_int | T_float | T_bool | T_string -> find_type_class_name e.etyp |> from_string
  | T_addr ->
    begin
      let addr = match ekind e with E_addr addr -> addr | _ -> assert false in
      match addr.addr_kind with
      | A_py_instance(cls, _) -> cls
      | _ -> assert false
    end
  | _ -> assert false

let from_expr exp =
  match ekind exp with
  | E_var v -> from_string v.vname
  | _ -> assert false

let eval_attribute name attr range =
  let addr = from_attribute name attr in
  mk_addr addr range



let fun_to_binop = function
  | "__add__" -> O_plus T_any
  | "__sub__" -> O_minus T_any
  | "__mul__" -> O_mult T_any
  | "__matmul__" -> O_py_mat_mult
  | "__truediv__" -> O_div T_any
  | "__floordiv__" -> O_py_floor_div
  | "__mod__" -> O_mod T_any
  | "__pow__" -> O_pow
  | "__lshift__" -> O_bit_lshift
  | "__rshift__" -> O_bit_rshift
  | "__and__" -> O_bit_and
  | "__xor__" -> O_bit_xor
  | "__or__" -> O_bit_or
  | "__eq__" -> O_eq
  | "__ne__" -> O_ne
  | "__lt__" -> O_lt
  | "__le__" -> O_le
  | "__gt__" -> O_gt
  | "__ge__" -> O_ge
  | "__radd__" -> O_plus T_any
  | "__rsub__" -> O_minus T_any
  | "__rmul__" -> O_mult T_any
  | "__rmatmul__" -> O_py_mat_mult
  | "__rtruediv__" -> O_div T_any
  | "__rfloordiv__" -> O_py_floor_div
  | "__rmod__" -> O_mod T_any
  | "__rpow__" -> O_pow
  | "__rlshift__" -> O_bit_lshift
  | "__rrshift__" -> O_bit_rshift
  | "__rand__" -> O_bit_and
  | "__rxor__" -> O_bit_xor
  | "__ror__" -> O_bit_or
  | "__iadd__" -> O_plus T_any
  | "__isub__" -> O_minus T_any
  | "__imul__" -> O_mult T_any
  | "__imatmul__" -> O_py_mat_mult
  | "__itruediv__" -> O_div T_any
  | "__ifloordiv__" -> O_py_floor_div
  | "__imod__" -> O_mod T_any
  | "__ipow__" -> O_pow
  | "__ilshift__" -> O_bit_lshift
  | "__irshift__" -> O_bit_rshift
  | "__iand__" -> O_bit_and
  | "__ixor__" -> O_bit_xor
  | "__ior__" -> O_bit_or
  | _ -> assert false

let binop_to_fun = function
  | O_plus T_any -> "__add__"
  | O_minus T_any -> "__sub__"
  | O_mult T_any -> "__mul__"
  | O_py_mat_mult -> "__matmul__"
  | O_div T_any -> "__truediv__"
  | O_py_floor_div -> "__floordiv__"
  | O_mod T_any -> "__mod__"
  | O_pow -> "__pow__"
  | O_bit_lshift -> "__lshift__"
  | O_bit_rshift -> "__rshift__"
  | O_bit_and -> "__and__"
  | O_bit_xor -> "__xor__"
  | O_bit_or -> "__or__"
  | O_eq -> "__eq__"
  | O_ne -> "__ne__"
  | O_lt -> "__lt__"
  | O_le -> "__le__"
  | O_gt -> "__gt__"
  | O_ge -> "__ge__"
  | _ -> assert false


let binop_to_rev_fun = function
  | O_plus T_any -> "__radd__"
  | O_minus T_any -> "__rsub__"
  | O_mult T_any -> "__rmul__"
  | O_py_mat_mult -> "__rmatmul__"
  | O_div T_any -> "__rtruediv__"
  | O_py_floor_div -> "__rfloordiv__"
  | O_mod T_any -> "__rmod__"
  | O_pow -> "__rpow__"
  | O_bit_lshift -> "__rlshift__"
  | O_bit_rshift -> "__rrshift__"
  | O_bit_and -> "__rand__"
  | O_bit_xor -> "__rxor__"
  | O_bit_or -> "__ror__"
  | _ -> assert false




let binop_to_incr_fun = function
  | O_plus T_any -> "__iadd__"
  | O_minus T_any -> "__isub__"
  | O_mult T_any -> "__imul__"
  | O_py_mat_mult -> "__imatmul__"
  | O_div T_any -> "__itruediv__"
  | O_py_floor_div -> "__ifloordiv__"
  | O_mod T_any -> "__imod__"
  | O_pow -> "__ipow__"
  | O_bit_lshift -> "__ilshift__"
  | O_bit_rshift -> "__irshift__"
  | O_bit_and -> "__iand__"
  | O_bit_xor -> "__ixor__"
  | O_bit_or -> "__ior__"
  | _ -> assert false


let is_binop_function = function
  | "__add__"
  | "__sub__"
  | "__mul__"
  | "__matmul__"
  | "__truediv__"
  | "__floordiv__"
  | "__mod__"
  | "__pow__"
  | "__lshift__"
  | "__rshift__"
  | "__and__"
  | "__xor__"
  | "__or__"
  | "__eq__"
  | "__ne__"
  | "__lt__"
  | "__le__"
  | "__gt__"
  | "__ge__"
  | "__iadd__"
  | "__isub__"
  | "__imul__"
  | "__imatmul__"
  | "__itruediv__"
  | "__ifloordiv__"
  | "__imod__"
  | "__ipow__"
  | "__ilshift__"
  | "__irshift__"
  | "__iand__"
  | "__ixor__"
  | "__ior__"
  | "__radd__"
  | "__rsub__"
  | "__rmul__"
  | "__rmatmul__"
  | "__rtruediv__"
  | "__rfloordiv__"
  | "__rmod__"
  | "__rpow__"
  | "__rlshift__"
  | "__rrshift__"
  | "__rand__"
  | "__rxor__"
  | "__ror__" -> true
  | _ -> false




let fun_to_unop = function
  | "__not__" -> O_log_not
  | "__neg__" -> O_minus T_any
  | "__pos__" -> O_plus T_any
  | "__invert__" -> O_bit_invert
  | _ -> assert false


let is_unop_function = function
  | "__not__"
  | "__neg__"
  | "__pos__"
  | "__invert__" -> true
  | _ -> false


let unop_to_fun = function
  | O_log_not -> "__not__"
  | O_plus T_any -> "__pos__"
  | O_minus T_any -> "__neg__"
  | O_bit_invert -> "__invert__"
  | _ -> assert false


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
