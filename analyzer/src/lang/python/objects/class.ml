(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handling of class definition and instantiation. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Framework.Pp
open Framework.Eval
open Framework.Exec
open Universal.Ast
open Ast
open Addr

let name = "python.objects.class"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let can_inherit_from exp =
    match ekind exp with
    | E_addr {addr_kind = A_py_class (C_builtin "bool", _)} ->
      false

    | E_addr {addr_kind = A_py_class (C_builtin "int", _) }
    | E_addr {addr_kind = A_py_class (C_builtin "float", _) }
    | E_addr {addr_kind = A_py_class (C_builtin "string", _) } ->
      Framework.Exceptions.panic "Inheritance from builtin classes not supported"

    | E_addr {addr_kind = A_py_class _ } ->
      true

    | _ ->
      false

  let exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_class cls ->
      debug "definition of class %a" pp_var cls.py_cls_var;
      eval_list cls.py_cls_bases (man.eval ctx) flow |>
      eval_to_exec
        (fun bases flow ->
           if List.for_all can_inherit_from bases then
             let bases =
               match bases with
               | [] -> [Builtins.object_addr]
               | _ ->
                 List.map (function {ekind = E_addr addr} -> addr | _ -> assert false)  bases
             in
             Universal.Utils.compose_alloc_exec
               (fun addr flow ->
                  let flow = man.exec ctx
                      (mk_assign
                         (mk_var cls.py_cls_var (tag_range range "class var"))
                         (mk_addr addr (tag_range range "class addr"))
                         (tag_range range "class addr assign")
                      ) flow
                  in
                  man.exec ctx cls.py_cls_body flow
               )
               (Addr.mk_class_addr cls bases) stmt.srange man ctx flow
           else
             man.exec ctx
               (Builtins.mk_builtin_raise "TypeError" (tag_range range "class def error")) flow
        )
        (man.exec ctx) man.flow  |>
      return

    | _ ->
      None


  let init _ ctx _ flow = ctx, flow


  let instantiate_object man ctx cls args range flow =
    debug "Instantiate class %a object" Universal.Pp.pp_addr cls;
    let tmp = mktmp () in

    (* Call __new__ *)
    let flow =
      man.exec ctx
        (let range = tag_range range "new" in
         mk_assign
           (mk_var tmp (tag_range range "tmp"))
           (mk_py_call
              (mk_py_addr_attr cls "__new__" (tag_range range "attr"))
              ((mk_addr cls (tag_range range "cls arg")) :: args)
              (tag_range range "call")
           )
           range
        )
        flow
    in

    (* Call __init__ *)
    (* FIXME: execute __init__ only if __new__ returned an instance of cls *)
    let flow =
      man.exec ctx
        (let range = tag_range range "init" in
         mk_stmt
           (S_expression(
               mk_py_call
                 (mk_py_addr_attr cls "__init__" (tag_range range "attr"))
                 ((mk_var tmp (tag_range range "obj")) :: args)
                 (tag_range range "call")
             ))
           range
        ) flow
    in
    (* FIXME: check that __init__ always returns None *)

    let evl = (Some (mk_var tmp range), flow, [mk_remove_var tmp (tag_range range "cleaner")]) in
    re_eval_singleton (man.eval ctx) evl


  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    (* Calls to classes to instantiate objects *)
    | E_py_call({ekind = E_addr ({addr_kind = A_py_class _} as cls)}, args, []) ->
      instantiate_object man ctx cls args range flow

    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
