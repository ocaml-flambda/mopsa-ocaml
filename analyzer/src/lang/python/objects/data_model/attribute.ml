(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Data model for attribute access. *)

open Framework.Domains.Global
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast
open Addr
open Utils


let name = "python.objects.data_model.attribute"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  (*==========================================================================*)
  (**                         {2 Lattice structure}                           *)
  (*==========================================================================*)

  (** Abstraction of objects attributes that were created dynamically. We maintain
      under and over approximations of relation pairs [addr * attr] to detected 
      TypeError exceptions. *)
  module AttrSet = Framework.Lattices.Range_set.Make
      (struct
        type t = addr * string
        let compare (addr1, attr1) (addr2, attr2) =
          compare_composer [
            (fun () -> compare_addr addr1 addr2);
            (fun () -> compare attr1 attr2);
          ]
        let print fmt (addr, attr) =
          Format.fprintf fmt "%a.%s" Universal.Pp.pp_addr addr attr
      end)

  include AttrSet

  (** Pretty printer of attributes abstraction. *)
  let print fmt abs =
    Format.fprintf fmt "attributes:@,@[<v2>  %a@]@\n"
      AttrSet.print abs

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init prog man flow =
    set_domain_cur AttrSet.empty man flow

  (** Check whether an attribute [attr] is an encoding of an abstract attribute
      introduced by the analyzer. *)
  let is_abstract_attribute attr =
    String.get attr 0 = '$'

  (** Check whether a type is atomic. *)
  let is_atomic_type = function
    | T_int
    | T_float
    | T_bool
    | T_string
    | T_py_none
    | T_py_not_implemented ->
      true
    | _ ->
      false

  let is_static_attribute addr attr =
    match addr.addr_kind with
    | A_py_class(C_user cls, _) ->
      List.exists (fun v -> v.vname = attr) cls.py_cls_static_attributes

    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) ->
      Builtins.is_builtin_attribute name attr

    | A_py_function _ | A_py_instance _ -> false
      
    | _ -> assert false
      
  let assume_is_attribute addr attr man ctx flow =
    if is_static_attribute addr attr then
      flow
    else
      map_domain_cur (fun a ->
          if AttrSet.may_mem (addr, attr) a then
            let _ = debug "attr may exist" in
            AttrSet.add (addr, attr) a
          else
            let _ = debug "attr does not exist" in
            AttrSet.bottom
        ) man flow

  let assume_is_not_attribute addr attr man ctx flow =
    if is_static_attribute addr attr then
      set_domain_cur bottom man flow
    else
      map_domain_cur (fun a ->
          if AttrSet.must_mem (addr, attr) a then
            let _ = debug "attr must exist" in
            AttrSet.bottom
          else
            let _ = debug "attr may not exist" in
            AttrSet.remove (addr, attr) a
        ) man flow

  let mk_static_attribute addr attr range =
    match addr.addr_kind with
    | A_py_class(C_user cls, _) ->
      let v = cls.py_cls_static_attributes |> List.find (fun v -> v.vname = attr) in
      mk_var v range

    | A_py_class(C_builtin name, _)
    | A_py_module(M_builtin name) ->
      Builtins.eval_attribute name attr range

    | _ ->
      assert false

  let mk_dynamic_attribute addr attr range =
    Universal.Ast.mk_addr_attribute addr attr range
  
  let mk_attribute_expr addr attr range =
    if is_static_attribute addr attr then
      mk_static_attribute addr attr range
    else
      mk_dynamic_attribute addr attr range
  
  let rec eval exp man ctx flow =
    let range = erange exp in
    match ekind exp with
    | E_py_attribute(obj, attr) ->
      (* Evaluate [obj] and check the resulting cases. *)
      Eval.compose_eval
        obj
        (fun obj flow ->
           match ekind obj with
           (* Access to an abstract attribute of an object *)
           | E_addr addr when is_abstract_attribute attr ->
             Eval.singleton (Some (Universal.Ast.mk_addr_attribute addr attr range), flow, [])

           (* Access to an ordinary attribute of an object *)
           | E_addr addr  ->
             eval_addr_attribute
               addr attr range
               (* Case when [addr.attr] evaluates to [exp] *)
               (fun exp flow ->
                  debug "attribute %s found, exp = %a" attr Framework.Pp.pp_expr exp;
                  Eval.re_eval_singleton man ctx (Some exp, flow, [])
               )
               (* Case when the attribute [attr] was not found *)
               (fun flow ->
                  let flow = man.exec
                      (Builtins.mk_builtin_raise "AttributeError" (tag_range range "error"))
                      ctx flow
                  in
                  Eval.singleton (None, flow, [])
               )
               man ctx flow

           (* Access to an ordinary attribute of atomic types *)
           | _ when etyp obj |> is_atomic_type ->
             assert false

           | _ -> assert false
        )
        (* Case when [obj] can not be evaluated *)
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow
    | _ -> None

    (** Evaluation of an attribute [attr] on a heap object. *)
  and eval_addr_attribute addr attr range found not_found man ctx flow =
    debug "eval access to attribute %s in address %a" attr Universal.Pp.pp_addr addr;
    match addr.addr_kind with
    (* Instances need particular processing in case of a class methods (for binding) *)
    | A_py_instance(cls, _) ->
      debug "accessing instance attribute";
      Eval.if_eval
        (assume_is_attribute addr attr man ctx flow)
        (assume_is_not_attribute addr attr man ctx flow)
        (* Case when the attribute is local to the instance*)
        (fun true_flow ->
           debug "instance attribute found locally";
           found (mk_attribute_expr addr attr range) true_flow
        )
        (* Case when the attribute does not exist => check in the class *)
        (fun false_flow ->
           debug "instance attribute not found locally";
           eval_addr_attribute cls attr range
             (fun exp flow -> assert false)
             not_found man ctx false_flow
        )
        (fun () -> Eval.singleton (None, flow, []))
        man flow

    (* General case *)
    | _ ->
      Eval.if_eval
        (assume_is_attribute addr attr man ctx flow)
        (assume_is_not_attribute addr attr man ctx flow)
        (* Case when the attribute exists statically or was created dynamically *)
        (fun true_flow ->
           debug "attr %s exists" attr;
           found (mk_attribute_expr addr attr range) true_flow
        )
        (* Case when the attribute does not exist *)
        (fun false_flow ->
           debug "attr %s not local, try further" attr;
           match addr.addr_kind with
           (* Check in mro *)
           | A_py_class(cls, bases) ->
             debug "trying mro";
             let rec aux = function
               | [] -> not_found flow
               | base :: tl ->
                 eval_addr_attribute base attr range found (fun flow -> aux tl) man ctx flow
             in aux bases

           | A_py_module(modl) ->
             debug "no mro";
             not_found flow

           | _ -> assert false
        )
        (fun () -> Eval.singleton (None, flow, []))
        man flow

  let exec stmt man ctx flow =
    match skind stmt with
    (* Assignments to an attribute of an object *)
    | Universal.Ast.S_assign({ekind = E_py_attribute(obj, attr)}, rval, kind) ->
      Eval.compose_exec_list
        [rval; obj]
        (fun el flow ->
           match el with
           | [rval; {ekind = E_addr obj; erange}] ->
             let lval, flow =
               if is_static_attribute obj attr then
                 mk_static_attribute obj attr erange, flow
               else
                 mk_dynamic_attribute obj attr erange, map_domain_cur (add (obj, attr)) man flow
             in
             man.exec (mk_assign ~kind lval rval stmt.srange) ctx flow |>
             Exec.return
           | _ ->
             man.exec (Builtins.mk_builtin_raise "AttributeError" (tag_range stmt.srange "error")) ctx flow |>
             Exec.return
               

        )
        (fun flow -> Exec.return flow)
        man ctx flow

    | _ ->
      None

 
  let ask query man ctx flow = None

end

let setup () =
  register_domain name (module Domain)
