(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of Python tuples. *)


open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Exec
open Framework.Domains.Stateless
open Framework.Flow
open Framework.Exceptions
open Universal.Ast
open Universal.Ast
open Utils
open Ast
open Addr

let name = "python.objects.containers.tuples"
let debug fmt = Debug.debug ~channel:name fmt

module Domain = struct

  (** Tuple length *)
  let mk_tl addr range = mk_py_addr_attr ~etyp:T_int addr "$tl" range

  (** Tuple value at index i *)
  let mk_tv addr i range = mk_py_addr_attr addr ("$tv[" ^ (string_of_int i) ^ "]") range

  (** Tuple value at index z *)
  let mk_tv_z addr z range = mk_py_addr_attr addr ("$tv[" ^ (Z.to_string z) ^ "]") range

  (** Iterator counter *)
  let mk_iter_counter addr range = mk_py_addr_attr addr "$counter" ~etyp:T_int range
          
  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_py_tuple(el)  ->
      Addr.eval_alloc_instance man ctx (Addr.find_builtin "tuple") None range flow |>
      oeval_compose (fun addr flow ->
          let tl = mk_tl addr range in
          let flow = man.exec ctx (mk_assign tl (mk_int (List.length el) range) range) flow in

          let rec aux i flow = function
            | [] -> flow
            | e :: tail ->
              let tv = mk_tv addr i range in
              let flow = man.exec ctx (mk_assign tv e range) flow in
              aux (i + 1) flow tail
          in

          let flow = eval_list el (man.eval ctx) flow |>
                     eval_to_exec (fun el flow -> aux 0 flow el) (man.exec ctx) man.flow
          in

          oeval_singleton (Some (mk_addr addr range), flow, [])
        )

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "tuple.__getitem__")})},
        [
          {ekind = E_addr {addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "tuple", _)}, _)}};
          idx
        ],
        []
      ) ->
      panic "tuple.__getitem__ not supported"
  
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "tuple.__init__")}},
        ({ekind = E_addr _} :: args),
        []
      )
      ->
      panic "tuple() not supported"

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "tuple.__len__")}},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "tuple", _)}, _)})}],
        []
      ) ->
      panic "tuple.__len__ not supported"
        
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "tuple.__iter__")})},
        [({ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "tuple", _)}, _)}  as tuple)})],
        []
      ) ->
      Addr.eval_alloc_instance man ctx (Addr.find_builtin "tupleiter") (Some (Tuple tuple)) range flow |>
      oeval_compose (fun aiter flow ->
          let flow = man.exec ctx (mk_assign (mk_iter_counter aiter range) (mk_zero range) range) flow in
          oeval_singleton (Some (mk_addr aiter range), flow, [])
        )
      
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "tupleiter.__next__")})},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "tupleiter", _)}, Some (Tuple tuple))}  as iter)}],
        []
      ) ->
      let tl = mk_tl tuple range in
      let counter = mk_iter_counter iter range in

      let in_cond = mk_in ~right_strict:true counter (mk_zero range) tl range in
      let in_flow = man.exec ctx (mk_assume in_cond range) flow in

      let in_case =
        if man.flow.is_cur_bottom in_flow then
          None
        else
          man.eval ctx counter in_flow |>
          eval_compose (fun counter flow ->
              match man.ask ctx (Universal.Numeric.Query.QIntInterval counter) flow with
              | Some itv ->
                debug "itv = %a" Universal.Numeric.Values.Int.print itv;
                if Universal.Numeric.Values.Int.is_bounded itv then
                  Universal.Numeric.Values.Int.fold (fun acc i ->
                      debug "assign counter";
                      let flow = man.exec ctx (mk_assign counter (mk_z (Z.succ i) range) range) flow in
                      let exp' = mk_tv_z tuple i range in
                      re_eval_singleton (man.eval ctx) (Some exp', flow, []) |>
                      oeval_join acc
                    ) None itv
                else
                  panic "Unbounded tuple not supported"

              | None -> assert false
            )

      in

      let out_cond = mk_binop counter O_eq tl range in
      let out_flow = man.exec ctx (mk_assume out_cond range) flow in
      let out_case =
        if man.flow.is_cur_bottom out_flow then
          None
        else
          let flow = man.exec ctx (Utils.mk_builtin_raise "StopIteration" range) out_flow
          in
          oeval_singleton (None, flow, [])

      in

      oeval_join in_case out_case

    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin f)})}, _, _)
      when Addr.is_builtin_class_function "tuple" f ->
      panic "Tuple function %s not implemented" f

    | _ -> None

  let init man ctx prog flow = ctx, flow
  
  let exec man ctx stmt flow = None

  let ask man ctx query flow = None

end

let setup () =
  register_domain name (module Domain)
