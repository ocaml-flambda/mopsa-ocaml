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

  let rec exec man ctx stmt flow =
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
               | _ -> List.map object_of_expr  bases
             in
             if Libs.Mopsa.is_builtin_clsdec cls then
               let name = Libs.Mopsa.builtin_clsdec_name cls in
               create_builtin_class (C_builtin name) name cls bases range;
               return flow
             else
             if Libs.Mopsa.is_unsupported_clsdec cls then
               let name = cls.py_cls_var.vname in
               create_builtin_class (C_unsupported name) name cls bases range;
               return flow
             else
               Addr.eval_alloc man ctx (A_py_class (C_user cls, bases)) stmt.srange flow |>
               oeval_to_oexec (fun obj flow ->
                   let flow = man.exec ctx
                       (mk_assign (mk_var cls.py_cls_var range) (mk_py_object obj range) range)
                       flow
                   in
                   man.exec ctx cls.py_cls_body flow |>
                   return
                 ) (man.exec ctx) man.flow
           else
             man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow |>
             return
        )
        (man.exec ctx) man.flow

    | _ -> None


  and create_builtin_class kind name cls bases range =
    let addr = {
      addr_kind = A_py_class(kind, bases);
      addr_range = range;
      addr_uid = 0;
    }
    in
    Addr.add_builtin_class (addr, None) ();

    (* Parse the body of the class *)
    let rec parse base stmt =
      match skind stmt with
      | S_py_class(cls) ->
        let name = mk_dot_name base cls.py_cls_var.vname in
        let bases = List.map (fun base ->
            match ekind base with
            | E_var v -> Addr.find_builtin v.vname
            | _ -> assert false
          ) cls.py_cls_bases
        in
        let kind =
          if Libs.Mopsa.is_unsupported_clsdec cls then C_unsupported name
          else C_builtin name
        in
        let addr = {
          addr_kind = A_py_class (kind, bases);
          addr_range = range;
          addr_uid = 0;
        }
        in
        Addr.add_builtin_class (addr, None) ();
        parse (Some name) cls.py_cls_body

      | S_py_function(fundec) ->
        let name = mk_dot_name base fundec.py_func_var.vname in
        let fundec = {fundec with py_func_var = {fundec.py_func_var with vname = name}} in
        let kind =
          if Libs.Mopsa.is_builtin_fundec fundec then F_builtin name else
          if Libs.Mopsa.is_unsupported_fundec fundec then F_unsupported name
          else F_user fundec
        in
        let addr = {
          addr_kind = A_py_function kind;
          addr_range = range;
          addr_uid = -1;
        }
        in
        Addr.add_builtin_function (addr, None) ()

      | S_block(block) ->
        List.iter (parse base) block

      | _ -> Framework.Exceptions.fail "stmt %a not supported in builtin class definition" Framework.Pp.pp_stmt stmt
    in
    parse (Some name) cls.py_cls_body


  let init _ ctx _ flow = ctx, flow


  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    (* Calls to classes to instantiate objects *)
    | E_py_call({ekind = E_py_object cls}, args, []) when Addr.isclass cls ->
      (* Call __new__ *)
      man.eval ctx (mk_py_call (mk_py_object_attr cls "__new__" range) ((mk_py_object cls range) :: args) range) flow |>
      eval_compose (fun eobj flow ->
          let obj = object_of_expr eobj in
          if Addr.isinstance obj cls then
            (* Call __init__ *)
            man.eval ctx (mk_py_call (mk_py_object_attr cls "__init__" range) (eobj :: args) range) flow |>
            eval_compose (fun r flow ->
                if is_none r then
                  oeval_singleton (Some eobj, flow, [])
                else
                  let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
                  oeval_singleton (None, flow, [])
               )
           else
             oeval_singleton (Some eobj, flow, [])
        )

    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
