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

  let is_static_attribute obj attr =
    match kind_of_object obj with
    | A_py_class(_) when Addr.is_object_unsupported obj ->
      Framework.Exceptions.panic "access to attribute of unsupported object %a" Pp.pp_py_object obj;

    | A_py_class(C_user cls, _) ->
      List.exists (fun v -> v.vname = attr) cls.py_cls_static_attributes

    | A_py_module(M_user(_, globals)) ->
      List.exists (fun v -> v.vname = attr) globals

    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) ->
      Addr.is_builtin_attribute obj attr


    | A_py_function _ | A_py_instance _ -> false

    | _ -> assert false

  let assume_is_attribute obj attr man ctx flow =
    debug "checking presence of attr %s in %a" attr Pp.pp_py_object obj;
    if is_static_attribute obj attr then
      flow
    else
      let addr = addr_of_object obj in
      map_domain_cur (fun a ->
          if AttrSet.may_mem (addr, attr) a then
            let _ = debug "attr may exist" in
            AttrSet.add (addr, attr) a
          else
            let _ = debug "attr does not exist" in
            AttrSet.bottom
        ) man flow

  let assume_is_not_attribute obj attr man ctx flow =
    if is_static_attribute obj attr then
      set_domain_cur bottom man flow
    else
      let addr = addr_of_object obj in
      map_domain_cur (fun a ->
          if AttrSet.must_mem (addr, attr) a then
            let _ = debug "attr must exist" in
            AttrSet.bottom
          else
            let _ = debug "attr may not exist" in
            AttrSet.remove (addr, attr) a
        ) man flow

  let mk_static_attribute obj attr range =
    match kind_of_object obj with
    | A_py_class(C_user cls, _) ->
      let v = cls.py_cls_static_attributes |> List.find (fun v -> v.vname = attr) in
      mk_var v range

    | A_py_module(M_user(name, globals)) when Addr.is_builtin_attribute obj attr ->
      let obj = Addr.find_builtin_attribute obj attr in
      mk_py_object obj range

    | A_py_module(M_user(name, globals))  ->
      let v = List.find (fun v -> v.vname = attr) globals in
      mk_var v range

    | A_py_class(C_builtin name, _) | A_py_module(M_builtin name) ->
      let obj = Addr.find_builtin_attribute obj attr in
      mk_py_object obj range

    | _ ->
      assert false

  let mk_attribute_var obj attr range =
    let addr = addr_of_object obj in
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


  let mk_attribute_expr obj attr etyp range =
    let e =
      if is_static_attribute obj attr then
        mk_static_attribute obj attr range
      else
        mk_attribute_var obj attr range
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
        (fun eobj flow ->
           let obj = object_of_expr eobj in
           if is_abstract_attribute attr then
             (* Access to an abstract attribute of an object *)
             let exp' = mk_attribute_expr obj attr exp.etyp range in
             re_eval_singleton (man.eval ctx) (Some exp', flow, [])
           else
             (* Access to an ordinary attribute of an object *)
             if_flow_eval
               (assume_is_attribute obj attr man ctx)
               (assume_is_not_attribute obj attr man ctx)
               (* Case when the attribute is local*)
               (fun true_flow ->
                  debug "instance attribute found locally";
                  debug "attribute %s found, exp = %a" attr Framework.Pp.pp_expr exp;
                  let exp' = mk_attribute_expr obj attr exp.etyp range in
                  re_eval_singleton (man.eval ctx) (Some exp', true_flow, [])
               )
               (* Case when the attribute does not exist => check in mro *)
               (fun false_flow ->
                  let mro = Addr.mro obj in
                  let rec aux flow = function
                    | [] ->
                      let flow = man.exec ctx (Utils.mk_builtin_raise "AttributeError" range) flow in
                      oeval_singleton (None, flow, [])

                    | cls :: tl ->
                      (* Check existence of the attribute in the class *)
                      if_flow_eval
                        (assume_is_attribute cls attr man ctx)
                        (assume_is_not_attribute cls attr man ctx)
                        (fun true_flow ->
                           (* Check method case *)
                           match kind_of_object obj with
                           (* In case of an instance, check that cls.attr is a function before binding it *)
                           | A_py_instance _ ->
                             man.eval ctx (mk_py_object_attr cls attr range) true_flow |>
                             eval_compose (fun eobj' flow ->
                                 let obj' = object_of_expr eobj' in
                                 (* Attribute is a function of the class => bound the method to the instance *)
                                 if Addr.isinstance obj' (Addr.find_builtin "function") then
                                   let exp = mk_expr (E_alloc_addr(A_py_method(obj', obj), range)) range in
                                   re_eval_singleton (man.eval ctx) (Some exp, true_flow, [])
                                 else
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
                   if Addr.is_builtin obj && Addr.is_builtin_attribute obj attr then
                     let exp' = mk_py_object (Addr.find_builtin_attribute obj attr) range in
                     oeval_singleton (Some exp', flow, [])
                   else
                     oeval_singleton (None, flow, [])
                 )
               man flow ()
        )

    (* Calls to hasattr *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "hasattr")}, _)}, [obj; attr], []) ->
      eval_list [obj; attr] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          (* FIXME: attr are no longer constants, but objects! *)
          let eobj, eattr = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          match Addr.type_of_object @@ object_of_expr eattr with
          | T_string ->
            let ev = value_of_object @@ object_of_expr eattr |> Option.none_to_exn in
            let s = man.ask ctx (Memory.Query.QString ev) flow |> Option.none_to_exn in
            if Memory.Value.S.is_top s then
              Framework.Exceptions.panic_at range "hasattr: top argument"
            else
            if Memory.Value.S.exists (
                function
                | "__dict__" | "__class__" | "__bases__" | "__name__"
                | "__qualname__"| "__mro__" | "mro" | "__subclass__" -> true
                | _ -> false
              ) s then
              Framework.Exceptions.panic_at range "calls to hasattr on special attributes not supported"
            else
              Memory.Value.S.fold (fun attr acc ->
                  let obj = object_of_expr eobj in
                  if_flow_eval
                    (assume_is_attribute obj attr man ctx)
                    (assume_is_not_attribute obj attr man ctx)
                    (fun true_flow -> oeval_singleton (Some (mk_py_true range), true_flow, []))
                    (fun false_flow ->
                       let mro = Addr.mro obj in
                       let rec aux flow = function
                         | [] -> oeval_singleton (Some (mk_py_false range), false_flow, [])
                         | cls :: tl ->
                           (* Check existence of the attribute in the class *)
                           if_flow_eval
                             (assume_is_attribute cls attr man ctx)
                             (assume_is_not_attribute cls attr man ctx)
                             (fun true_flow -> oeval_singleton (Some (mk_py_true range), true_flow, []))
                             (fun false_flow -> aux false_flow tl)
                             man flow ()
                       in
                       aux false_flow mro
                    )
                    man flow () |>
                  oeval_join acc
                ) s None                

          | _ ->
            let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
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
           | [rval; {ekind = E_py_object obj; erange}] ->
             let lval, flow =
               if is_static_attribute obj attr then
                 mk_static_attribute obj attr erange, flow
               else
                 let addr = addr_of_object obj in
                 mk_attribute_var obj attr erange, map_domain_cur (add (addr, attr)) man flow
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
