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


(*==========================================================================*)
(**                           {2 Addresses}                                 *)
(*==========================================================================*)


(** Parameters of instances of some builtin-in types *)
type obj_param =
  | List of Universal.Ast.addr (* the address of the iterated list used by listiter *)
  | Tuple of Universal.Ast.addr
  | Dict of Universal.Ast.addr
  | Range of Universal.Ast.addr
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
  | A_py_class of class_address (** class *) * addr list (** bases *)
  | A_py_function of function_address (** function *)
  | A_py_instance of Universal.Ast.addr (** class of the instance *) * obj_param option (** optional parameters *)
  | A_py_method of Universal.Ast.addr (** address of the function to bind *) * Universal.Ast.addr (** bound instance *)
  | A_py_method_atomic of Universal.Ast.addr (** address of the function to bind *) * expr (** bound atomic value *)
  | A_py_module of module_address


(** Allocate an object on the heap and return its address as an evaluation *)
let eval_alloc (man: ('a, 't) manager) ctx kind range flow : (addr, 'a) evals option =
  let exp = mk_alloc_addr kind range range in
  man.eval ctx exp flow |>
  eval_compose (fun exp flow ->
      match ekind exp with
      | E_addr addr -> oeval_singleton (Some addr, flow, [])
      | _ -> Framework.Exceptions.panic "eval_alloc: allocation returned a non-address express %a" Framework.Pp.pp_expr exp
    )


(** Allocate an instance and return its address as an evaluation *)
let eval_alloc_instance (man: ('a, 't) manager) ctx cls params range flow : (addr, 'a) evals option =
  eval_alloc man ctx (A_py_instance(cls, params)) range flow


(*==========================================================================*)
(**                           {2 Built-ins}                                 *)
(*==========================================================================*)


(** Lists of built-ins *)
let classes : addr list ref = ref []
let functions : addr list ref = ref []
let modules : addr list ref = ref []
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

(** Name of an address *)
let addr_name addr =
  match addr.addr_kind with
  | A_py_class(C_builtin name, _) | A_py_class(C_unsupported name, _)
  | A_py_function(F_builtin name) | A_py_function(F_unsupported name)
  | A_py_module(M_builtin name) | A_py_module(M_user (name, _))
    -> name
  | A_py_function(F_user f) -> f.py_func_var.vname
  | A_py_class(C_user c, _) -> c.py_cls_var.vname
  | _ -> Framework.Exceptions.fail "builtin_name: %a is not a builtin" Universal.Pp.pp_addr addr


let add_builtin_class addr () =
  classes := addr :: !classes

let add_builtin_function addr () =
  functions := addr :: !functions

let add_builtin_module addr () =
  modules := addr :: !modules

(** Search for the address of a builtin given its name *)
let find_builtin name =
  debug "searching for builtin %s" name;
  List.find (fun addr ->
      name = addr_name addr
    ) (all ())

let is_unsupported addr =
  match addr.addr_kind with
  | A_py_class(C_unsupported _, _)
  | A_py_function (F_unsupported _) -> true
  | _ -> false


(** Search for the address of a builtin attribute *)
let find_builtin_attribute base attr =
  let name = addr_name base in
  if is_unsupported base then
    Framework.Exceptions.panic "Unsupported builtin %s" name
  else
    match base.addr_kind with
    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) | A_py_module(M_user (name, _)) ->
      find_builtin (mk_dot_name (Some name) attr)
    | A_py_class(C_user cls, _) ->
      let name = cls.py_cls_var.vname in
      find_builtin (mk_dot_name (Some name) attr)
    | _ -> assert false

(** Check whether a built-in exists given its name *)
let is_builtin name = List.exists (fun addr -> name = addr_name addr) (all ())

let is_builtin_addr addr = List.mem addr (all ())

(** Check whether an attribute of a built-in object exists, given its name *)
let is_builtin_attribute base attr =
  let name = addr_name base in
  if is_unsupported base then
    Framework.Exceptions.panic "Unsupported builtin %s" name
  else
    match base.addr_kind with
    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) ->
      is_builtin (mk_dot_name (Some name) attr)
    | _ -> false

(** Check whether a dot-named function [f] is a member of the class [cls] *)
let is_builtin_class_function cls f =
  match split_dot_name f with
  | None -> false
  | Some (cls', _) -> cls = cls' && is_builtin f


(** Name of an atomic type *)
let atomaic_type_to_class_name = function
  | T_int -> "int"
  | T_float -> "float"
  | T_bool -> "bool"
  | T_string -> "str"
  | T_py_none -> "NoneType"
  | T_py_complex -> "complex"
  | T_py_not_implemented -> "NotImplementedType"
  | _ -> assert false



(*==========================================================================*)
(**                      {2 Utility functions}                              *)
(*==========================================================================*)


(** Address of the (type) class of an expression *)
let classof addr =
  debug "classof %a" Universal.Pp.pp_addr addr;
  match addr.addr_kind with
  | A_py_instance(cls, _) -> cls
  | A_py_class _ -> find_builtin "type"
  | A_py_function _ -> find_builtin "function"
  | A_py_method _ -> find_builtin "method"
  | A_py_module _ -> find_builtin "module"
  | _ -> assert false


let rec mro addr =
  debug "mro of %a" Universal.Pp.pp_addr addr;
  let l = match addr.addr_kind with
    | A_py_class(_, bases) -> addr :: (List.map mro bases |> List.flatten)
    | A_py_instance(cls, _) -> mro cls
    | _ -> assert false
  in
  debug "|mro| = %d" (List.length l);
  l

(** Return the closest non-heap (i.e. non-user defined) base class of
   an address *)
let most_derive_builtin_base addr =
  let rec aux =
    function
    | [a] when is_builtin_addr a -> a
    | a :: tl when is_builtin_addr a -> a
    | a :: tl -> aux tl
    | [] -> assert false
  in
  aux (mro addr)

(** Check class inheritance  *)
let issubclass cls1 cls2 =
  match cls1.addr_kind, cls2.addr_kind with
  | A_py_class _, A_py_class (C_builtin "type", _)-> true
  | A_py_class _, A_py_class _ ->
    debug "issubclass %a %a" Universal.Pp.pp_addr cls1 Universal.Pp.pp_addr cls2;
    let b = List.exists (fun base -> compare_addr base cls2 = 0) (mro cls1) in
    debug "b = %b" b;
    b
  | _ -> false

(** Check class membership of an instance *)
let isinstance obj cls =
  debug "isinstance %a %a" Universal.Pp.pp_addr obj Universal.Pp.pp_addr cls;
  match obj.addr_kind, cls.addr_kind with
  | A_py_instance(cls', _), A_py_class _ -> issubclass cls' cls
  | A_py_instance(cls', _),  _ -> false
  | A_py_class _, A_py_class (C_builtin "type", _)-> true
  | A_py_class _, _ -> false
  | _ -> assert false


(** Unique allocation range for atomic values *)
let common_range = mk_file_range "_mopsa_stdlib.py"

(** Compute the address of an atomic type *)
let of_atomic_type ~weak t range =
  let cls = atomaic_type_to_class_name t |> find_builtin in
  {
    addr_kind = A_py_instance(cls, None);
    addr_range = range;
    addr_uid = if weak then Universal.Heap.Pool.old_uid else Universal.Heap.Pool.recent_uid;
  }

let has_atomic_type addr =
  let cls = classof addr in
  let builtin_base = most_derive_builtin_base cls in
  match builtin_base.addr_kind with
  | A_py_class (C_builtin "int", _)
  | A_py_class (C_builtin "float", _)
  | A_py_class (C_builtin "bool", _)
  | A_py_class (C_builtin "complex", _)
  | A_py_class (C_builtin "str", _)
  | A_py_class (C_builtin "NoneType", _)
  | A_py_class (C_builtin "NotImplementedType", _) -> true
  | _ -> false

(** Atomic type of an address *)
let to_atomic_type a =
  let cls = classof a in
  let builtin_base = most_derive_builtin_base cls in
  match builtin_base.addr_kind with
  | A_py_class (C_builtin "int", _) -> T_int
  | A_py_class (C_builtin "float", _) -> T_float
  | A_py_class (C_builtin "bool", _) -> T_bool
  | A_py_class (C_builtin "complex", _) -> T_py_complex
  | A_py_class (C_builtin "str", _) -> T_string
  | A_py_class (C_builtin "NoneType", _) -> T_py_none
  | A_py_class (C_builtin "NotImplementedType", _) -> T_py_not_implemented
  | _ -> Framework.Exceptions.fail "%a is not an address of an atomic type" Universal.Pp.pp_addr a

(** Address of a constant *)
let of_constant c range =
  match c with
  | C_int _ | C_int_interval _ -> of_atomic_type ~weak:true T_int range
  | C_float _ | C_float_interval _ -> of_atomic_type ~weak:true T_float range
  | C_py_imag _ -> of_atomic_type ~weak:true T_py_complex range
  | C_true | C_false -> of_atomic_type ~weak:false T_bool common_range
  | C_string _ -> of_atomic_type ~weak:true T_string range
  | C_py_none -> of_atomic_type ~weak:false T_py_none common_range
  | C_py_not_implemented -> of_atomic_type ~weak:false T_py_not_implemented common_range
  | C_top t -> of_atomic_type ~weak:true t range
  | _ -> assert false


let () =
  Universal.Pp.(
    Format.(
      register_pp_addr (fun default fmt a ->
          match a.addr_kind, Universal.Heap.Recency.is_weak a with
          | A_py_class(C_user c, _), _ -> fprintf fmt "{%a}" pp_var c.py_cls_var
          | A_py_class((C_builtin c | C_unsupported c), _), _ -> fprintf fmt "{%s}" c
          | A_py_function(F_user f), _ -> fprintf fmt "function %a" pp_var f.py_func_var
          | A_py_function((F_builtin f | F_unsupported f)), _ -> fprintf fmt "function %s" f
          | A_py_instance(c, _), false -> fprintf fmt "<%a object @@ %a>" pp_addr c pp_range a.addr_range
          | A_py_instance(c, _), true -> fprintf fmt "<%a object w@@ %a>" pp_addr c pp_range a.addr_range
          | A_py_method(f, obj), _ -> fprintf fmt "method %a of %a" pp_addr f pp_addr obj
          | A_py_method_atomic(f, obj), _ -> fprintf fmt "method %a of %a}" pp_addr f pp_expr obj
          | A_py_module(M_user(m, _) | M_builtin(m)), _ -> fprintf fmt "module %s" m
          | _ -> default fmt a
        )
    )
  )
