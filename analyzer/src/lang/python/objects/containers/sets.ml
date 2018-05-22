(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of Python mutable sets. *)

open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Domains.Stateless
open Framework.Flow
open Framework.Exceptions
open Universal.Ast
open Universal.Ast
open Utils
open Ast
open Addr

let name = "python.objects.containers.sets"
let debug fmt = Debug.debug ~channel:name fmt

module Domain = struct

  (** Auxiliary variable for storing the values of a set *)
  let mk_sv addr range = mk_py_addr_attr addr "$sv" range

  (** Auxiliary variable representing the emptiness of a set *)
  let mk_se addr range = mk_py_addr_attr addr "$se" ~etyp:T_bool range

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "set.__init__")}},
        [{ekind = E_addr set}],
        []
      )
      ->
      let se = mk_se set range in
      let sv = mk_sv set range in

      let flow = man.exec ctx (mk_assign se (mk_true range) range) flow |>
                 man.exec ctx (mk_assign sv (mk_empty range) range)
      in

      oeval_singleton (Some (mk_py_none range), flow, [])

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "set.__init__")}},
        [{ekind = E_addr set}; iterable],
        []
      )
      ->
      let se = mk_se set range in

      let flow = man.exec ctx (mk_assign se (mk_true range) range) flow in

      man.eval ctx (Utils.mk_builtin_call "iter" [iterable] range) flow |>
      eval_compose (fun iter flow ->
          let next = Utils.mk_builtin_call "next" [iter] range in
          let flow = man.exec ctx
              (Utils.mk_try_stopiteration
                 (mk_while 
                    (mk_true range)
                    (mk_stmt (S_expression (Utils.mk_builtin_call "set.add" [mk_addr set range; next] range)) range)
                    range
                 )
                 (mk_block [] range)
                 range
              )
              flow
          in
          oeval_singleton (Some (mk_py_none range), flow, [])
        )

    | E_py_set(el) ->
      panic "set not supported"

    | E_py_set_comprehension(e, comprhs) ->
      panic "set comprehension not supported"

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "set.add")}},
        [{ekind = E_addr set}; value],
        []
      ) ->

      let sv = mk_sv set range in
      let se = mk_se set range in

      let empty_flow = man.exec ctx (mk_assume se range) flow in
      let empty_case =
        if man.flow.is_cur_bottom empty_flow then
          None
        else
          let flow = man.exec ctx (mk_assign sv value range) empty_flow |>
                     man.exec ctx (mk_assign se (mk_false range) range)
          in
          oeval_singleton (Some (mk_py_none range), flow, [])
      in

      let non_empty_flow = man.exec ctx (mk_assume (mk_not se range) range) flow in
      let non_empty_case =
        if man.flow.is_cur_bottom non_empty_flow then
          None
        else
          let flow = man.exec ctx (mk_assign sv value range) non_empty_flow |>
                     man.flow.join non_empty_flow
          in
          oeval_singleton (Some (mk_py_none range), flow, [])
      in

      oeval_join empty_case non_empty_case

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "set.clear")}},
        [{ekind = E_addr set}],
        []
      ) ->

      let sv = mk_sv set range in
      let se = mk_se set range in

      let flow = man.exec ctx (mk_assign sv (mk_empty range) range) flow |>
                 man.exec ctx (mk_assign se (mk_true range) range)
      in
      oeval_singleton (Some (mk_py_none range), flow, [])

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin f)})}, _, _)
      when Addr.is_builtin_class_function "set" f ->
      panic "Set function %s not implemented" f
        
    | _ -> None

  let init man ctx prog flow = ctx, flow
  
  let exec man ctx stmt flow = None

  let ask man ctx query flow = None

end

let setup () =
  register_domain name (module Domain)
