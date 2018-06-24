(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Heap addresses of Python objects. *)

open Framework.Essentials
open Universal.Ast
open Ast

let debug fmt = Debug.debug ~channel:"python.addr" fmt


(*==========================================================================*)
(**                           {2 Addresses}                                 *)
(*==========================================================================*)


(** Parameters of instances of some builtin-in types *)
type obj_param =
  | List of py_object (* the address of the iterated list used by listiter *)
  | Tuple of py_object
  | Dict of py_object
  | Range of py_object
  | Generator of py_fundec

(** Classes *)
type class_address =
  | C_builtin of string (* name of a built-in class *)
  | C_user of py_clsdec (* declaration of a user class *)
  | C_unsupported of string (** unsupported class *)

(** Functions *)
type function_address =
  | F_builtin of string (* name of a builtin function *)
  | F_user of py_fundec (* declaration of a user function *)
  | F_unsupported of string (** unsupported function *)

type module_address =
  | M_user of string (** name *) * var list (** globals *)
  | M_builtin of string (** name *)

(** Kinds of Python addresses *)
type Universal.Ast.addr_kind +=
  | A_py_class of class_address (** class *) * py_object list (** bases *)
  | A_py_function of function_address (** function *)
  | A_py_instance of py_object (** class of the instance *) * obj_param option (** optional parameters *)
  | A_py_method of py_object (** address of the function to bind *) * py_object (** method instance *)
  | A_py_module of module_address


let mk_alloc_instance cls ?(params = None) range =
  Universal.Ast.mk_alloc_addr (A_py_instance(cls, params)) range range


(*==========================================================================*)
(**                           {2 Built-ins}                                 *)
(*==========================================================================*)


(** Lists of built-ins *)
let classes : py_object list ref = ref []
let functions : py_object list ref = ref []
let modules : py_object list ref = ref []
let all () = !classes @ !functions @ !modules

(** Name of a builtin with an optional dot notation in case of
   sub-objects (methods of classes, etc.) *)
let mk_dot_name base name =
  match base with
  | None -> name
  | Some base -> base ^ "." ^ name

(** Return the base and the attribute of a dot name *)
let split_dot_name x =
  let l = String.split_on_char '.' x in
  match l with
  | [cls; attr] -> Some (cls, attr)
  | [modul; cls; attr] -> Some (modul ^ "." ^ cls, attr)
  | _ -> None

(** Address of an object *)
let addr_of_object (obj: py_object) : Universal.Ast.addr = fst obj

let kind_of_object (obj: py_object) : Universal.Ast.addr_kind =
  let addr = addr_of_object obj in
  addr.addr_kind

(** Name of an object *)
let object_name obj =
  match kind_of_object obj with
  | A_py_class(C_builtin name, _) | A_py_class(C_unsupported name, _)
  | A_py_function(F_builtin name) | A_py_function(F_unsupported name)
  | A_py_module(M_builtin name) | A_py_module(M_user (name, _))
    -> name
  | A_py_function(F_user f) -> f.py_func_var.vname
  | A_py_class(C_user c, _) -> c.py_cls_var.vname
  | _ -> Framework.Exceptions.fail "builtin_name: %a is not a builtin" Universal.Pp.pp_addr (addr_of_object obj)

let add_builtin_class obj () =
  classes := obj :: !classes

let add_builtin_function obj () =
  functions := obj :: !functions

let add_builtin_module obj () =
  modules := obj :: !modules

(** Search for the address of a builtin given its name *)
let find_builtin name =
  List.find (fun obj ->
      name = object_name obj
    ) (all ())

let is_object_unsupported obj =
  match kind_of_object obj with
  | A_py_class(C_unsupported _, _)
  | A_py_function (F_unsupported _) -> true
  | _ -> false

(** Check whether a built-in exists given its name *)
let is_builtin_name name = List.exists (fun obj -> name = object_name obj) (all ())

let is_builtin obj = List.mem obj (all ())

(** Check whether an attribute of a built-in object exists, given its name *)
let is_builtin_attribute base attr =
  let name = object_name base in
  if is_object_unsupported base then
    Framework.Exceptions.panic "Unsupported builtin %s" name
  else
    match kind_of_object base with
    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) ->
      is_builtin_name (mk_dot_name (Some name) attr)
    | _ -> false

(** Search for the address of a builtin attribute *)
let find_builtin_attribute base attr =
  let name = object_name base in
  if is_object_unsupported base then
    Framework.Exceptions.panic "Unsupported builtin %s" name
  else
    match kind_of_object base with
    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) | A_py_module(M_user (name, _)) ->
      find_builtin (mk_dot_name (Some name) attr)
    | A_py_class(C_user cls, _) ->
      let name = cls.py_cls_var.vname in
      find_builtin (mk_dot_name (Some name) attr)
    | _ -> assert false


(** Check whether a dot-named function [f] is a member of the class [cls] *)
let is_builtin_class_function cls f =
  match split_dot_name f with
  | None -> false
  | Some (cls', _) -> cls = cls' && is_builtin_name f


(** Class name of an atomic type *)
let atomaic_type_to_class_name (t: Framework.Ast.typ) : string=
  let open Universal.Ast in
  match t with
  | T_int -> "int"
  | T_float -> "float"
  | T_py_bool -> "bool"
  | T_string -> "str"
  | T_py_none -> "NoneType"
  | T_py_complex -> "complex"
  | T_py_not_implemented -> "NotImplementedType"
  | _ -> assert false



(*==========================================================================*)
(**                      {2 Utility functions}                              *)
(*==========================================================================*)


(** Address of the type class of an object *)
let class_of_object (obj: py_object) : py_object =
  match kind_of_object obj with
  | A_py_instance(cls, _) -> cls
  | A_py_class _ -> find_builtin "type"
  | A_py_function _ -> find_builtin "function"
  | A_py_method _ -> find_builtin "method"
  | A_py_module _ -> find_builtin "module"
  | _ -> assert false

(** Method resolution order of an object *)
let rec mro (obj: py_object) : py_object list =
  match kind_of_object obj with
  | A_py_class(_, bases) -> obj :: (List.map mro bases |> List.flatten)
  | A_py_instance(cls, _) -> mro cls
  | _ -> assert false
    
(** Return the closest non-heap (i.e. non-user defined) base class *)
let most_derive_builtin_base (obj: py_object) : py_object =
  let rec aux =
    function
    | [o] when is_builtin o -> o
    | o :: tl when is_builtin o -> o
    | o :: tl -> aux tl
    | [] -> assert false
  in
  aux (mro obj)

(** Check class inheritance  *)
let issubclass (cls1: py_object) (cls2: py_object) : bool =
  match kind_of_object cls1, kind_of_object cls2 with
  | A_py_class _, A_py_class (C_builtin "type", _)-> true

  | A_py_class _, A_py_class _ ->
    List.exists (fun base -> compare_py_object base cls2 = 0) (mro cls1)

  | _ -> false

(** Check class membership of an instance *)
let isinstance obj cls =
  match kind_of_object obj, kind_of_object cls with
  | A_py_instance(cls', _), A_py_class _ -> issubclass cls' cls
  | A_py_instance(cls', _),  _ -> false
  | A_py_class _, A_py_class (C_builtin "type", _)-> true
  | A_py_class _, _ -> false
  | _ -> assert false

let isclass obj =
  match kind_of_object obj with
  | A_py_class _ -> true
  | _ -> false

(** Atomic type of an address *)
let is_atomic_object obj =
  let cls = class_of_object obj in
  let builtin_base = most_derive_builtin_base cls in
  let open Universal.Ast in
  match kind_of_object builtin_base with
  | A_py_class (C_builtin "int", _)
  | A_py_class (C_builtin "float", _)
  | A_py_class (C_builtin "bool", _)
  | A_py_class (C_builtin "complex", _)
  | A_py_class (C_builtin "str", _)
  | A_py_class (C_builtin "NoneType", _)
  | A_py_class (C_builtin "NotImplementedType", _) -> true
  | _ -> false

let type_of_object obj =
  let cls = class_of_object obj in
  let builtin_base = most_derive_builtin_base cls in
  let open Universal.Ast in
  match kind_of_object builtin_base with
  | A_py_class (C_builtin "int", _) -> T_int
  | A_py_class (C_builtin "float", _) -> T_float
  | A_py_class (C_builtin "bool", _) -> T_py_bool
  | A_py_class (C_builtin "complex", _) -> T_py_complex
  | A_py_class (C_builtin "str", _) -> T_string
  | A_py_class (C_builtin "NoneType", _) -> T_py_none
  | A_py_class (C_builtin "NotImplementedType", _) -> T_py_not_implemented
  | _ -> T_py_empty

let is_weak obj =
  let addr = addr_of_object obj in
  Universal.Heap.Recency.is_weak addr

let is_not_implemented r =
  let o = object_of_expr r in
  isinstance o (find_builtin "NotImplementedType")

let is_none r =
  let o = object_of_expr r in
  isinstance o (find_builtin "NoneType")

let none_range = Framework.Location.mk_fresh_range ()
let mk_py_none range =
  let addr = Universal.Ast.{
      addr_kind = A_py_instance (find_builtin "NoneType", None);
      addr_range = none_range;
      addr_uid = Universal.Heap.Recency.strong_addr_uid;
    }
  in
  let e = mk_constant ~etyp:T_py_none C_py_none range in
  mk_py_object (addr, e) range


let not_implemented_range = Framework.Location.mk_fresh_range ()
let mk_py_not_implemented range =
  let addr = Universal.Ast.{
      addr_kind = A_py_instance (find_builtin "NotImplementedType", None);
      addr_range = not_implemented_range;
      addr_uid = Universal.Heap.Recency.strong_addr_uid;
    }
  in
  let e = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
  mk_py_object (addr, e) range

let int_range = Framework.Location.mk_fresh_range ()
let mk_py_int_expr e range =
  let addr = Universal.Ast.{
      addr_kind = A_py_instance (find_builtin "int", None);
      addr_range = int_range;
      addr_uid = Universal.Heap.Recency.weak_addr_uid;
    }
  in
  mk_py_object (addr, e) range

let mk_py_z z range = mk_py_int_expr (Universal.Ast.mk_z z range) range
let mk_py_z_interval z1 z2 range = mk_py_int_expr (Universal.Ast.mk_z_interval z1 z2 range) range
let mk_py_int n range = mk_py_z (Z.of_int n) range
let mk_py_zero range = mk_py_z Z.zero range
let mk_py_one range = mk_py_z Z.one range

let float_range = Framework.Location.mk_fresh_range ()
let mk_py_float_expr e range =
  let addr = Universal.Ast.{
      addr_kind = A_py_instance (find_builtin "float", None);
      addr_range = float_range;
      addr_uid = Universal.Heap.Recency.weak_addr_uid;
    }
  in
  mk_py_object (addr, e) range

let mk_py_float f range = mk_py_float_expr (Universal.Ast.mk_float f range) range
let mk_py_float_interval f1 f2 range = mk_py_int_expr (Universal.Ast.mk_float_interval f1 f2 range) range

let bool_range = Framework.Location.mk_fresh_range ()
let mk_py_bool_expr e range =
  let addr = Universal.Ast.{
      addr_kind = A_py_instance (find_builtin "bool", None);
      addr_range = bool_range;
      addr_uid = Universal.Heap.Recency.weak_addr_uid;
    }
  in
  mk_py_object (addr, e) range

let mk_py_true range = mk_py_bool_expr (Universal.Ast.mk_one range) range
let mk_py_false range = mk_py_bool_expr (Universal.Ast.mk_zero range) range

let string_range = Framework.Location.mk_fresh_range ()
let mk_py_string_expr e range =
  let addr = Universal.Ast.{
      addr_kind = A_py_instance (find_builtin "str", None);
      addr_range = string_range;
      addr_uid = Universal.Heap.Recency.weak_addr_uid;
    }
  in
  mk_py_object (addr, e) range

let mk_py_string s range = mk_py_string_expr (Universal.Ast.mk_string s range) range

let complex_range = Framework.Location.mk_fresh_range ()
let mk_py_imag j range =
  let addr = Universal.Ast.{
      addr_kind = A_py_instance (find_builtin "complex", None);
      addr_range = complex_range;
      addr_uid = Universal.Heap.Recency.weak_addr_uid;
    }
  in
  let e = mk_constant ~etyp:T_py_complex (C_py_imag j) range in
  mk_py_object (addr, e) range

let mk_py_top t range =
  let open Universal.Ast in
  match t with
  | T_py_bool -> mk_py_bool_expr (mk_int_interval 0 1 range) range
  | T_int -> mk_py_int_expr (Framework.Ast.mk_top T_int range) range
  | T_float -> mk_py_float_expr (Framework.Ast.mk_top T_float range) range
  | T_string -> mk_py_string_expr (Framework.Ast.mk_top T_string range) range
  | _ -> assert false

let mk_py_constant c range =
  let open Universal.Ast in
  match c with
  | C_int z -> mk_py_z z range
  | C_float f -> mk_py_float f range
  | C_string s -> mk_py_string s range
  | C_py_none -> mk_py_none range
  | C_py_not_implemented -> mk_py_not_implemented range
  | C_py_imag j -> mk_py_imag j range
  | _ -> Framework.Exceptions.panic_at range "mk_py_constant: unknown constant %a" pp_constant c

let () =
  Universal.Pp.(
    Format.(
      register_pp_addr (fun default fmt a ->
          match a.addr_kind, Universal.Heap.Recency.is_weak a with
          | A_py_class(C_user c, _), _ -> fprintf fmt "{%a}" pp_var c.py_cls_var
          | A_py_class((C_builtin c | C_unsupported c), _), _ -> fprintf fmt "{%s}" c
          | A_py_function(F_user f), _ -> fprintf fmt "function %a" pp_var f.py_func_var
          | A_py_function((F_builtin f | F_unsupported f)), _ -> fprintf fmt "function %s" f
          | A_py_instance(c, _), false -> fprintf fmt "<%a object @@ %a>" pp_addr (addr_of_object c) Framework.Location.pp_range a.addr_range
          | A_py_instance(c, _), true -> fprintf fmt "<%a object w@@ %a>" pp_addr (addr_of_object c) Framework.Location.pp_range a.addr_range
          | A_py_method(f, obj), _ -> fprintf fmt "method %a of %a" pp_addr (addr_of_object f) pp_addr (addr_of_object obj)
          | A_py_module(M_user(m, _) | M_builtin(m)), _ -> fprintf fmt "module %s" m
          | _ -> default fmt a
        )
    )
  )
