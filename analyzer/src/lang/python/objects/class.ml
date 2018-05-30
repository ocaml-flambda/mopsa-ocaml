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
      eval_to_oexec
        (fun bases flow ->
           if List.for_all can_inherit_from bases then
             let bases =
               match bases with
               | [] -> [Addr.find_builtin "object"]
               | _ ->
                 List.map (function {ekind = E_addr addr} -> addr | _ -> assert false)  bases
             in
             let kind =
               if Libs.Mopsa.is_unsupported_clsdec cls then C_unsupported cls.py_cls_var.vname
               else C_user cls
             in
             Addr.eval_alloc man ctx (A_py_class (kind, bases)) stmt.srange flow |>
             oeval_to_oexec (fun addr flow ->
                 let flow = man.exec ctx
                     (mk_assign
                        (mk_var cls.py_cls_var range)
                        (mk_addr addr range)
                        range
                     ) flow
                 in
                 man.exec ctx cls.py_cls_body flow |>
                 return
               ) (man.exec ctx) man.flow
           else
             man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow |>
             return
        )
        (man.exec ctx) man.flow

    | _ ->
      None


  let init _ ctx _ flow = ctx, flow


  let instantiate_object man ctx cls args range flow =
    debug "Instantiate class %a object" Universal.Pp.pp_addr cls;
    let tmp = mktmp () in

    (* Call __new__ *)
    let flow =
      man.exec ctx
        (mk_assign
           (mk_var tmp range)
           (mk_py_call
              (mk_py_addr_attr cls "__new__" range)
              ((mk_addr cls range) :: args)
              range
           )
           range
        )
        flow
    in

    (* Call __init__ *)
    (* FIXME: execute __init__ only if __new__ returned an instance of cls *)
    let flow =
      man.exec ctx
        (mk_stmt
           (S_expression(
               mk_py_call
                 (mk_py_addr_attr cls "__init__" range)
                 ((mk_var tmp range) :: args)
                 range
             ))
           range
        ) flow
    in
    (* FIXME: check that __init__ always returns None *)

    let evl = (Some (mk_var tmp range), flow, [mk_remove_var tmp range]) in
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
