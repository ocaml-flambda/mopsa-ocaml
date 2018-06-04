(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Data model for attribute access. *)

open Framework.Domains.Stateful
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Exec
open Framework.Ast
open Universal.Ast
open Ast
open Addr
open Utils


let name = "python.data_model.attribute"
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

  let init man ctx prog flow =
    ctx, set_domain_cur AttrSet.empty man flow

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
    | A_py_class(_) when Addr.is_unsupported addr ->
      Framework.Exceptions.panic "access to attribute of unsupported object %a" Universal.Pp.pp_addr addr;

    | A_py_class(C_user cls, _) ->
      List.exists (fun v -> v.vname = attr) cls.py_cls_static_attributes

    | A_py_module(M_user(_, globals)) ->
      List.exists (fun v -> v.vname = attr) globals

    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) ->
      Addr.is_builtin_attribute addr attr


    | A_py_function _ | A_py_instance _ -> false

    | _ -> assert false

  let assume_is_attribute addr attr man ctx flow =
    debug "checking presence of attr %s in %a" attr Universal.Pp.pp_addr addr;
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

    | A_py_module(M_user(name, globals)) ->
      begin
        try
          let addr = Addr.find_builtin_attribute addr attr in
          mk_addr addr range
        with Not_found ->
          let v = List.find (fun v -> v.vname = attr) globals in
          mk_var v range
      end

    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) ->
      let addr = Addr.find_builtin_attribute addr attr in
      mk_addr addr range


    | _ ->
      assert false

  let mk_attribute_var addr attr range =
    let v = {
      vname = (
        let () = Format.fprintf Format.str_formatter "%a.%s" Universal.Pp.pp_addr addr attr in
        let name = Format.flush_str_formatter () in
        name
      );
      vuid = 0;
      vtyp = T_any;
      vkind = V_orig;
    }
    in
    mk_var v range


  let mk_attribute_expr addr attr etyp range =
    let e =
      if is_static_attribute addr attr then
        mk_static_attribute addr attr range
      else
        mk_attribute_var addr attr range
    in
    {e with etyp}

  let rec eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    (* Special attributes *)
    | E_py_attribute(obj, ("__dict__" as attr))
    | E_py_attribute(obj, ("__class__" as attr))
    | E_py_attribute(obj, ("__bases__" as attr))
    | E_py_attribute(obj, ("__name__" as attr))
    | E_py_attribute(obj, ("__qualname__" as attr))
    | E_py_attribute(obj, ("__mro__" as attr))
    | E_py_attribute(obj, ("mro" as attr))
    | E_py_attribute(obj, ("__subclass__" as attr)) ->
      Framework.Exceptions.panic_at range "Access to special attribute %s not supported" attr

    (* Other attributes *)
    | E_py_attribute(obj, attr) ->
      (* Evaluate [obj] and check the resulting cases. *)
      man.eval ctx obj flow |>
      eval_compose
        (fun obj flow ->
           match ekind obj with
           (* Access to an abstract attribute of an object *)
           | E_addr addr when is_abstract_attribute attr ->
             let exp' = mk_attribute_expr addr attr exp.etyp range in
             re_eval_singleton (man.eval ctx) (Some exp', flow, [])

           (* Access to an ordinary attribute of an object *)
           | E_addr addr  ->
             if_flow_eval
               (assume_is_attribute addr attr man ctx)
               (assume_is_not_attribute addr attr man ctx)
               (* Case when the attribute is local*)
               (fun true_flow ->
                  debug "instance attribute found locally";
                  debug "attribute %s found, exp = %a" attr Framework.Pp.pp_expr exp;
                  let exp' = mk_attribute_expr addr attr exp.etyp range in
                  re_eval_singleton (man.eval ctx) (Some exp', true_flow, [])
               )
               (* Case when the attribute does not exist => check in mro *)
               (fun false_flow ->
                  let mro = Addr.mro addr in
                  let rec aux flow = function
                    | [] ->
                      let flow = man.exec ctx
                          (Utils.mk_builtin_raise "AttributeError" range)
                          flow
                      in
                      oeval_singleton (None, flow, [])

                    | cls :: tl ->
                      (* Check existence of the attribute in the class *)
                      if_flow_eval
                        (assume_is_attribute cls attr man ctx)
                        (assume_is_not_attribute cls attr man ctx)
                        (fun true_flow ->
                           (* Check method case *)
                           match addr.addr_kind with
                           (* In case of an instance, check that cls.attr is a function before binding it *)
                           | A_py_instance _ ->
                             man.eval ctx (mk_py_addr_attr cls attr range) true_flow |>
                             eval_compose (fun f flow ->
                                 match ekind f with
                                 (* Attribute is a function of the class => bound the method to the instance *)
                                 | E_addr ({addr_kind = A_py_function _} as f) ->
                                   let exp = mk_expr (E_alloc_addr(A_py_method(f, addr), range)) range in
                                   re_eval_singleton (man.eval ctx) (Some exp, true_flow, [])

                                 | _ ->
                                   let exp = mk_attribute_expr cls attr exp.etyp range in
                                   re_eval_singleton (man.eval ctx) (Some exp, true_flow, [])
                               )

                           (* No method binding for non-instances *)
                           | _ ->
                             let exp = mk_attribute_expr cls attr exp.etyp range in
                             re_eval_singleton (man.eval ctx) (Some exp, true_flow, [])
                        )
                        (fun false_flow -> aux false_flow tl)
                        man flow ()
                  in
                  aux false_flow mro
               )
               ~bottom_case:(fun () ->
                   (* In a bottom environment, the only thing that we
                      can do is to search for builtins attributes and
                      resolve them statically *)
                   if Addr.is_builtin_addr addr && Addr.is_builtin_attribute addr attr then
                     let exp' = mk_addr (Addr.find_builtin_attribute addr attr) range in
                     oeval_singleton (Some exp', flow, [])
                   else
                     oeval_singleton (None, flow, [])
                 )
               man flow ()

           (* Access to an ordinary attribute of atomic types *)
           | _ when etyp obj |> is_atomic_type ->
             debug "resolving attribute %s of atomic type exp %a" attr Framework.Pp.pp_expr obj;
             let cls = Addr.classof @@ addr_of_expr obj in
             debug "class %a" Universal.Pp.pp_addr cls;
             if_flow_eval
               (assume_is_attribute cls attr man ctx)
               (assume_is_not_attribute cls attr man ctx)
               (fun true_flow ->
                  (* Check method case *)
                  debug "has attribute";
                  man.eval ctx (mk_py_addr_attr cls attr range) true_flow |>
                  eval_compose (fun f flow ->
                      debug "attribute evaluated to %a" Framework.Pp.pp_expr f;
                      match ekind f with
                      (* Attribute is a function of the class => bound the method to the instance *)
                      | E_addr ({addr_kind = A_py_function _} as f) ->
                        let exp = mk_expr (E_alloc_addr(A_py_method_atomic(f, obj), range)) range in
                        re_eval_singleton (man.eval ctx) (Some exp, true_flow, [])

                      | _ ->
                        let exp = mk_attribute_expr cls attr exp.etyp range in
                        re_eval_singleton (man.eval ctx) (Some exp, true_flow, [])
                    )
               )
               (fun false_flow ->
                  debug "attribute not found";
                  let flow = man.exec ctx
                      (Utils.mk_builtin_raise "AttributeError" range)
                      false_flow
                  in
                  oeval_singleton (None, flow, [])
               )
               man flow ()

           | _ -> assert false
        )

    (* Calls to hasattr *)
    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "hasattr")}}, [obj; attr], []) ->
      eval_list [obj; attr] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          match el with
          | [_; {ekind = E_constant (C_string  ("__dict__" as attr))}]
          | [_; {ekind = E_constant (C_string  ("__class__" as attr))}]
          | [_; {ekind = E_constant (C_string  ("__bases__" as attr))}]
          | [_; {ekind = E_constant (C_string  ("__name__" as attr))}]
          | [_; {ekind = E_constant (C_string  ("__qualname__" as attr))}]
          | [_; {ekind = E_constant (C_string  ("__mro__" as attr))}]
          | [_; {ekind = E_constant (C_string  ("mro" as attr))}]
          | [_; {ekind = E_constant (C_string  ("__subclass__" as attr))}] ->
            Framework.Exceptions.panic_at range "calls to hasattr on special attribute %s not supported" attr

          | [{ekind = E_addr addr}; {ekind = E_constant (C_string attr)}] ->
            if_flow_eval
              (assume_is_attribute addr attr man ctx)
              (assume_is_not_attribute addr attr man ctx)
              (fun true_flow -> oeval_singleton (Some (mk_true range), true_flow, []))
              (fun false_flow ->
                 let mro = Addr.mro addr in
                 let rec aux flow = function
                   | [] -> oeval_singleton (Some (mk_false range), false_flow, [])
                   | cls :: tl ->
                     (* Check existence of the attribute in the class *)
                     if_flow_eval
                       (assume_is_attribute cls attr man ctx)
                       (assume_is_not_attribute cls attr man ctx)
                       (fun true_flow -> oeval_singleton (Some (mk_true range), true_flow, []))
                       (fun false_flow -> aux false_flow tl)
                       man flow ()
                 in
                 aux false_flow mro
              )
              man flow ()

          | [v; {ekind = E_constant (C_string attr)}] when etyp v |> is_atomic_type->
            let cls = Addr.classof @@ addr_of_expr obj in
            if_flow_eval
              (assume_is_attribute cls attr man ctx)
              (assume_is_not_attribute cls attr man ctx)
              (fun true_flow -> oeval_singleton (Some (mk_true range), true_flow, []))
              (fun false_flow -> oeval_singleton (Some (mk_false range), false_flow, []))
              man flow ()

          | _ ->
            let flow = man.exec ctx
                (Utils.mk_builtin_raise "TypeError" range)
                flow
            in
            oeval_singleton (None, flow, [])
        )
    | _ -> None

  let exec man ctx stmt flow =
    match skind stmt with
    (* Assignments to an attribute of an object *)
    | Universal.Ast.S_assign({ekind = E_py_attribute(obj, attr)}, rval, mode) ->
      eval_list [rval; obj] (man.eval ctx) flow |>
      eval_to_exec
        (fun el flow ->
           match el with
           | [rval; {ekind = E_addr obj; erange}] ->
             let lval, flow =
               if is_static_attribute obj attr then
                 mk_static_attribute obj attr erange, flow
               else
                 mk_attribute_var obj attr erange, map_domain_cur (add (obj, attr)) man flow
             in
             man.exec ctx (mk_assign ~mode lval rval stmt.srange) flow
           | _ ->
             man.exec ctx (Utils.mk_builtin_raise "AttributeError" stmt.srange) flow
        )
        (man.exec ctx) man.flow  |>
      return

    | _ ->
      None


  let ask man ctx query flow = None

end

let setup () =
  register_domain name (module Domain)
