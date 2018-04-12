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
open Framework.Utils
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
      
  let exec stmt manager ctx flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_class cls ->
      debug "definition of class %a" pp_var cls.py_cls_var;
      man_eval_list cls.py_cls_bases manager ctx flow |>
      oeval_to_exec
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
                  let flow = manager.exec
                      (mk_assign
                         (mk_var cls.py_cls_var (tag_range range "class var"))
                         (mk_addr addr (tag_range range "class addr"))
                         (tag_range range "class addr assign")
                      ) ctx flow
                  in
                  manager.exec cls.py_cls_body ctx flow |>
                  return
               )
               (Addr.mk_class_addr cls bases) stmt.srange manager ctx flow
           else
             manager.exec
               (Builtins.mk_builtin_raise "TypeError" (tag_range range "class def error"))
               ctx flow |>
             return
        )
        manager ctx

    | _ ->
      None


  let init _ _ ctx flow = ctx, flow


  let instantiate_object cls args range manager ctx flow =
    debug "Instantiate class %a object" Universal.Pp.pp_addr cls;
    let tmp = mktmp () in

    (* Call __new__ *)
    let flow =
      manager.exec
        (let range = tag_range range "new" in
         mk_assign
           (mk_var tmp (tag_range range "tmp"))
           (mk_py_call
              (mk_py_attr cls "__new__" (tag_range range "attr"))
              ((mk_addr cls (tag_range range "cls arg")) :: args)
              (tag_range range "call")
           )
           range
        )
        ctx flow
    in
    
    (* Call __init__ *)
    (* FIXME: execute __init__ only if __new__ returned an instance of cls *)
    let flow = 
      manager.exec
        (let range = tag_range range "init" in
         mk_stmt
           (S_expression(
               mk_py_call
                 (mk_py_attr cls "__init__" (tag_range range "attr"))
                 ((mk_var tmp (tag_range range "obj")) :: args)
                 (tag_range range "call")
             ))
           range
        ) ctx flow
    in
    (* FIXME: check that __init__ always returns None *)
    
    let evl = (Some (mk_var tmp range), flow, [mk_remove_var tmp (tag_range range "cleaner")]) in
    re_eval_singleton evl manager ctx

    
  let eval exp manager ctx flow =
    let range = erange exp in
    match ekind exp with
    (* Calls to classes to instantiate objects *)
    | E_py_call({ekind = E_addr ({addr_kind = A_py_class _} as cls)}, args, []) ->
      instantiate_object cls args range manager ctx flow
        
    | _ -> None
      
  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
