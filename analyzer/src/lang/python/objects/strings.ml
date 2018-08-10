(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Methods of class str. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.objects.strings"

module Domain = struct

  let eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "str.__new__")}}, [cls], []) ->
      oeval_singleton (Some (mk_string "" range), flow, [])


    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "str.__new__")}}, [cls; {etyp = T_string} as arg], []) ->
      oeval_singleton (Some arg, flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "str.__new__")}}, [cls; arg], []) ->
      (* FIXME: call repr when necessary *)
      let cls = classof arg in
      let exp = mk_py_call (mk_py_addr_attr cls "__str__" range) [arg] range in
      man.eval ctx exp flow |>
      eval_compose (fun exp flow ->
          match etyp exp with
          | T_string -> oeval_singleton (Some exp, flow, [])
          | _ ->
            let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
            oeval_singleton (None, flow, [])
        )

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "str.__len__")}}, [{ekind = E_constant (C_string s)}], []) ->
      oeval_singleton (Some (mk_int (String.length s) range), flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "str.__len__")}}, [{etyp = T_string} as arg], []) ->
      let s = man.ask ctx (Memory.Nonrel.Domain.QEval arg) flow |> OptionExt.none_to_exn in
      if Memory.Value.(S.is_top s.string) then
        Framework.Exceptions.panic "str.__len__ on top"
      else
        Memory.Value.S.fold (fun s acc ->
            oeval_join acc
              (oeval_singleton (Some (mk_int (String.length s) range), flow, []))
          ) s.Memory.Value.string None

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "str.__eq__")}}, [{etyp=T_string} as e1; {etyp=T_string} as e2], []) ->
      let exp' = mk_binop e1 O_eq e2 ~etyp:T_bool range in
      oeval_singleton (Some exp', flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "str.__ne__")}}, [{etyp=T_string} as e1; {etyp=T_string} as e2], []) ->
      let exp' = mk_binop e1 O_ne e2 ~etyp:T_bool range in
      oeval_singleton (Some exp', flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "str.__eq__")}}, [_; _], []) ->
      oeval_singleton (Some (mk_py_not_implemented range), flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "str.splitlines")}}, [e], []) ->

      begin
        match ekind e with
        | E_constant (C_string s) ->
          let l = String.split_on_char '\n' s in
          let el = List.map (fun s -> mk_string s range) l in
          re_eval_singleton (man.eval ctx) (Some {exp with ekind = E_py_list(el)}, flow, [])

        | _ ->
          Framework.Exceptions.panic "str.splitlines on non-string litterals not supported"
      end

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin f)})}, _, _)
      when Addr.is_builtin_class_function "str" f ->
      Framework.Exceptions.panic "String function %s not implemented" f

    | _ -> None


  let init _ ctx _ flow = ctx, flow

  let exec man ctx stmt flow = None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
