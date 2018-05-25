(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of Python range objects. *)

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

let name = "python.objects.containers.ranges"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let mk_start addr range = mk_py_addr_attr addr "$start" ~etyp:T_int range
  let mk_stop addr range = mk_py_addr_attr addr "$stop" ~etyp:T_int range
  let mk_counter addr range = mk_py_addr_attr addr "$counter" ~etyp:T_int range

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "range.__new__")}},
        cls :: args,
        []
      ) ->

      let estart, estop=
        match args with
        | [{etyp = T_int} as start; {etyp = T_int} as stop] -> start, stop
        | [{etyp = T_int} as stop] -> mk_zero range, stop
        | _ -> panic "range not supported"
      in

      man.eval ctx (mk_py_call (mk_py_addr_attr (Addr.find_builtin "object") "__new__" range) [cls] range) flow |>
      eval_compose (fun obj flow ->
          match ekind obj with
          | E_addr addr ->
            let start = mk_start addr range in
            let stop = mk_stop addr range in

            let flow = man.exec ctx (mk_assign start estart range) flow |>
                       man.exec ctx (mk_assign stop estop range)
            in
            oeval_singleton (Some (mk_addr addr range), flow, [])

          | _ -> assert false
        )

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin("range.__len__"))}},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "range", _)}, _)} as arange)}],
        []
      ) ->

      let start = mk_start arange range in
      let stop = mk_stop arange range in
      let exp' = mk_binop stop math_minus start ~etyp:T_int range in
      let cond = mk_binop exp' O_ge (mk_zero range) range in

      let flow_pos = man.exec ctx (mk_assume cond range) flow in
      let flow_neg = man.exec ctx (mk_assume (mk_not cond range) range) flow in

      oeval_join
        (re_eval_singleton (man.eval ctx) (Some exp', flow_pos, []))
        (re_eval_singleton (man.eval ctx) (Some (mk_zero range), flow_neg, []))

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin("range.__iter__"))})},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "range", _)}, _)} as arange)}],
        []
      ) ->

      eval_alloc_instance man ctx (Addr.find_builtin "rangeiter") (Some (Range arange)) range flow |>
      oeval_compose (fun iter flow ->
          let counter = mk_counter iter range in
          let flow = man.exec ctx (mk_assign counter (mk_zero range) range) flow in
          oeval_singleton (Some (mk_addr iter range), flow, [])
        )

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin("rangeiter.__next__"))})},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "rangeiter", _)}, Some (Range arange))} as aiter)}],
        []
      ) ->
      let start = mk_start arange range in
      let stop = mk_stop arange range in
      let counter = mk_counter aiter range in
      let cond =
        mk_in ~right_strict:true
          (mk_binop counter math_plus start ~etyp:T_int range)
          start
          stop
          range
      in

      let cond' = mk_not cond range in

      let flow_in = man.exec ctx (mk_assume cond range) flow in
      let flow_out = man.exec ctx (mk_assume cond' range) flow in

      let flow_in' = man.exec ctx (mk_assign counter (mk_binop counter math_plus (mk_one range) ~etyp:T_int range) range) flow_in in
      let flow_out' = man.exec ctx (Utils.mk_builtin_raise "StopIteration" range) flow_out in

      oeval_join
        (re_eval_singleton (man.eval ctx) (Some (mk_binop counter math_minus (mk_one range) ~etyp:T_int range), flow_in', []))
        (oeval_singleton (None, flow_out', []))


    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin("range.__contains__"))}},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "range", _)}, _)} as arange)};
         {etyp = T_int} as value],
        []
      ) ->
      begin

        let start = mk_start arange range in
        let stop = mk_stop arange range in

        let can_be_true =
          not @@ man.flow.is_cur_bottom @@
          man.exec ctx (mk_assume (mk_in ~right_strict:true value start stop range) range) flow
        in

        debug "can be true = %b" can_be_true;

        debug "check false";

        let can_be_false =
          not @@ man.flow.is_cur_bottom @@
          man.exec ctx (mk_assume (mk_not (mk_in ~right_strict:true value start stop range) range) range) flow
        in

        debug "can be false = %b" can_be_false;

        match can_be_true, can_be_false with
        | true, false -> oeval_singleton (Some (mk_true range), flow, [])
        | false, true -> oeval_singleton (Some (mk_false range), flow, [])
        | true, true -> oeval_join
                          (oeval_singleton (Some (mk_true range), flow, []))
                          (oeval_singleton (Some (mk_false range), flow, []))
        | false, false -> oeval_singleton (None, flow, [])

      end

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin("range.__contains__"))}},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "range", _)}, _)})}; _],
        []
      ) ->
      panic "range.__contains__ on non-ints not supported"

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin f)})}, _, _)
      when Addr.is_builtin_class_function "range" f ->
      panic "Range function %s not implemented" f

    | _ -> None

  let init _ ctx _ flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain)
