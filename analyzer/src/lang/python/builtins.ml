(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Language builtins. *)

open Framework.Ast
open Universal.Ast
open Ast
open Addr


(* let debug fmt = Format.kasprintf (fun str ->
 *     Format.printf "%s@." str
 *   ) fmt *)

let common_builtin_range = mk_fresh_range ()

let builtin_range name = tag_range common_builtin_range "%s" name

let object_addr = {
  addr_kind = A_py_class (C_builtin "object", []);
  addr_range = builtin_range "object";
  addr_uid = -1;
}

let modules = ref []
let classes = ref [object_addr]
let functions = ref []
let exceptions = ref []

let all () = !modules @ !classes @ !functions @ !exceptions

let rec builtin_addr_to_name addr =
  match addr.addr_kind with
  | A_py_class(C_builtin(name), _) | A_py_function(F_builtin(name)) | A_py_module(M_builtin(name)) ->
    name

  | A_py_method(cls, func) -> (builtin_addr_to_name cls) ^ "." ^ (builtin_addr_to_name func)

  | _ -> assert false

let from_string name =
  List.find (fun addr -> name = builtin_addr_to_name addr) (all ())

let mk_unname base name =
  match base with
  | None -> name
  | Some base -> base ^ "." ^ name

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

let mk_builtin_raise exn range = assert false

let mk_builtin_call f params range =
  mk_py_call (mk_addr (from_string f) range) params range

let mk_hasattr obj attr range =
  mk_builtin_call "hasattr" [obj; mk_string attr range] range

let find_type_class_name = function
  | T_int -> "int"
  | T_float -> "float"
  | T_bool -> "bool"
  | T_string -> "str"
  | _ -> assert false

let find_type_function t f =
  let cls = find_type_class_name t in
  from_attribute cls f

let mro name = assert false

let from_expr exp =
  match ekind exp with
  | E_var v -> from_string v.vname
  | _ -> assert false

let eval_attribute name attr range =
  let addr = from_attribute name attr in
  mk_addr addr range

let mk_builtin_module_addr modl =
  let range = builtin_range modl in
  {
    addr_kind = A_py_module (M_builtin modl);
    addr_range = range;
    addr_uid = -1;
  }


let mk_builtin_class_addr base cls =
  let name = mk_unname base cls.py_cls_var.vname in
  let bases = List.map from_expr cls.py_cls_bases in
  let range = builtin_range name in
  {
    addr_kind = A_py_class (C_builtin name, bases);
    addr_range = range;
    addr_uid = -1;
  }

let mk_builtin_function_addr base fundec =
  let name = mk_unname base fundec.py_func_var.vname in
  let range = builtin_range name in
  {
    addr_kind = A_py_function (F_builtin name);
    addr_range = range;
    addr_uid = -1;
  }



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




let rec parse_functions base stmt =
  match skind stmt with
  | S_py_function(fundec) ->
    functions := (mk_builtin_function_addr base fundec) :: !functions

  | S_block(block) ->
    List.iter (parse_functions base) block

  | _ -> ()


let rec parse_classes modl stmt =
  match skind stmt with
  | S_py_class(cls) ->
    let addr = mk_builtin_class_addr modl cls in
    classes :=  addr :: !classes;
    let base = mk_unname modl cls.py_cls_var.vname in
    parse_functions (Some base) cls.py_cls_body

  | S_block(block) ->
    List.iter (parse_classes modl) block

  | _ -> ()

let rec parse_exceptions stmt =
  match skind stmt with
  | S_py_class(cls) ->
    let addr = mk_builtin_class_addr None cls in
    exceptions :=  addr :: !exceptions

  | S_block(block) ->
    List.iter parse_exceptions block

  | _ -> ()


let init_std_classes f =
  let body = Frontend.parse_file f in
  parse_classes None body

let init_std_functions f =
  let body = Frontend.parse_file f in
  parse_functions None body

let init_std_exceptions f =
  let body = Frontend.parse_file f in
  parse_exceptions body

let init_std dir =
  let fclass = dir ^ "/std_classes.py" in
  init_std_classes fclass;

  let ffun = dir ^ "/std_functions.py" in
  init_std_functions ffun;

  let fexn = dir ^ "/std_exceptions.py" in
  init_std_exceptions fexn;
  ()

let init_module filename =
  let body = Frontend.parse_file filename in
  let modl = Filename.basename filename |> Filename.remove_extension in
  modules := (mk_builtin_module_addr modl) :: !modules;
  parse_classes (Some modl) body;
  parse_functions (Some modl) body

let init_modules dir =
  Sys.readdir dir |>
  Array.iter (fun stub ->
      let filename = Filename.basename stub in
      if filename <> "std_classes.py" && filename <> "std_functions.py" && filename <> "std_exceptions.py" then
        init_module (dir ^ "/" ^ stub)
      else
        ()
    )

let setup () =
  Framework.Options.(common_options.stubs) |> List.iter
    (fun dir ->
       init_std dir;
       init_modules dir;
    );
  ()
