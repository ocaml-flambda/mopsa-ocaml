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
  | AssumedExn of Universal.Ast.addr

(** Classes *)
type class_address =
  | C_builtin of string (* name of a built-in class *)
  | C_user of py_clsdec (* declaration of a user class *)

(** Functions *)
type function_address =
  | F_builtin of string (* name of a builtin function *)
  | F_user of py_fundec (* declaration of a user function *)

(** Modules *)
type module_address =
  | M_builtin of string (* name of a builtin module *)

(** Kinds of Python addresses *)
type Universal.Ast.addr_kind +=
  | A_py_class of class_address (** class *) * addr list (** bases *)
  | A_py_function of function_address (** function *)
  | A_py_instance of Universal.Ast.addr (** class of the instance *) * obj_param option (** optional parameters *)
  | A_py_method of Universal.Ast.addr (** address of the function to bind *) * Universal.Ast.addr (** bound instance *)
  | A_py_module of module_address (** module *)


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
let modules : addr list ref = ref []
let classes : addr list ref = ref []
let functions : addr list ref = ref []
let exceptions : addr list ref = ref []
let all () = !modules @ !classes @ !functions @ !exceptions

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

(** Name of a builtin address *)
let builtin_name addr =
  match addr.addr_kind with
  | A_py_class(C_builtin name, _)
  | A_py_function(F_builtin name)
  | A_py_module(M_builtin name) -> name
  | _ -> Framework.Exceptions.fail "builtin_name: %a is not a builtin" Universal.Pp.pp_addr addr
      
(** Search for the address of a builtin given its name *)
let find_builtin name =
  debug "searching for builtin %s" name;
  List.find (fun addr ->
      name = builtin_name addr
    ) (all ())

(** Search for the address of an attribute of a builtin, given its name *)
let find_builtin_attribute obj attr =
  let base = find_builtin obj in
  match base.addr_kind with
  | A_py_module(M_builtin name)
  | A_py_class(C_builtin name, _) ->
    find_builtin (mk_dot_name (Some name) attr)

  | _ -> assert false

(** Check whether a built-in exists given its name *)
let is_builtin name = List.exists (fun addr -> name = builtin_name addr) (all ())

(** Check whether a built-in module exists given its name *)
let is_builtin_module name = List.exists (fun addr -> name = builtin_name addr) !modules

(** Check whether an attribute of a built-in object exists, given its name *)
let is_builtin_attribute name attr =
  let base = find_builtin name in
  match base.addr_kind with
  | A_py_module(M_builtin name)
  | A_py_class(C_builtin name, _) ->
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
  | _ -> assert false

(** Address of the (type) class of an expression *)
let classof e =
  debug "classof %a(%a)" Framework.Pp.pp_expr e Framework.Pp.pp_typ e.etyp;
  match etyp e with
  | T_int | T_float | T_bool | T_string -> atomaic_type_to_class_name e.etyp |> find_builtin
  | T_addr ->
    begin
      let addr = match ekind e with E_addr addr -> addr | _ -> assert false in
      match addr.addr_kind with
      | A_py_instance(cls, _) -> cls
      | _ -> assert false
    end
  | _ -> assert false


let rec mro addr =
  debug "mro of %a" Universal.Pp.pp_addr addr;
  let l = match addr.addr_kind with
    | A_py_class(C_user cls, bases) -> addr :: (List.map mro bases |> List.flatten)
    | A_py_class(C_builtin cls, bases) -> (find_builtin cls) :: (List.map mro bases |> List.flatten)
    | A_py_instance(cls, _) -> mro cls
    | _ -> assert false
  in
  debug "|mro| = %d" (List.length l);
  l


(** Check class inheritance  *)
let issubclass cls1 cls2 =
  List.exists (fun base -> compare_addr base cls2 = 0) (mro cls1)

(** Check class membership of an instance *)
let isinstance obj cls =
  match obj.addr_kind, cls.addr_kind with
  | A_py_instance(cls', _), A_py_class _ ->
    issubclass cls' cls

  | A_py_class _, A_py_class (C_builtin "type", _)-> true

  | A_py_class _, _ -> false

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
          | _ -> default fmt ak
        )
    )
  )
