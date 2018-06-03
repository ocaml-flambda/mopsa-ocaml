(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Methods of number classes: int, float and bool. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.objects.number"
let debug fmt = Debug.debug ~channel:name fmt

module Domain= struct

  let is_number_function f =
    match Addr.split_dot_name f with
    | Some (cls, _) ->
      (cls = "int" || cls = "float" || cls = "bool")
    | None -> false

  let is_number_binop_function f =
    match Addr.split_dot_name f with
    | Some (cls, f) ->
      (cls = "int" || cls = "float" || cls = "bool") && (Operators.is_binop_function f)
    | None -> false

  let is_number_unop_function f =
    match Addr.split_dot_name f with
    | Some (cls, f) ->
      (cls = "int" || cls = "float" || cls = "bool") && (Operators.is_unop_function f)
    | None -> false


  let is_binop_valid op t1 t2 =
    match op with
    | O_eq | O_ne -> true
    | _ ->
      match t2, t2 with
      | T_bool, T_bool
      | T_int, T_int
      | T_float, T_float

      | T_int, T_bool
      | T_bool, T_int
      | T_float, T_int
      | T_int, T_float
      | T_float, T_bool
      | T_bool, T_float -> true

      | _ -> false

  let coerce op t1 t2 =
    match op with
    | O_eq | O_ne | O_lt | O_le | O_gt | O_ge ->
      T_bool
    | O_div T_any -> T_float
    | _ ->
      match t1, t2 with
      | T_bool, T_bool ->
        begin
          match op with
          | O_plus T_any | O_minus T_any | O_mult T_any | O_py_floor_div | O_bit_lshift | O_bit_rshift -> T_int
          | O_bit_and | O_bit_or | O_bit_xor -> T_bool
          | _ -> assert false
        end
      | T_int, T_int
      | T_int, T_bool
      | T_bool, T_int -> T_int

      | T_float, T_float
      | T_float, T_int
      | T_int, T_float
      | T_float, T_bool
      | T_bool, T_float -> T_float

      | _ -> assert false

  let change_operator_type t = function
    | O_plus _ -> O_plus t
    | O_minus _ -> O_minus t
    | O_div _ -> O_div t
    | O_mult _ -> O_mult t
    | O_mod _ -> O_mod t
    | op -> op

  let rec eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    (** Boleans *)
    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "bool.__new__")}}, [cls], []) ->
      oeval_singleton (Some (mk_false range), flow, [])


    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "bool.__new__")}}, [cls; arg], [])
      ->
      man.eval ctx arg flow |>
      eval_compose (fun arg flow ->
          match ekind arg with
          | E_constant (C_py_none)
          | E_constant (C_py_not_implemented)
          | E_constant (C_false) ->
            oeval_singleton (Some (mk_false range), flow, [])
          | E_constant (C_true) ->
            oeval_singleton (Some (mk_true range), flow, [])
          | _ ->
            let cls = Addr.classof arg in
            let bool_case =
              let flow = man.exec ctx (mk_assume (Utils.mk_builtin_call "hasattr" [arg; mk_string "__bool__" range] range) range) flow in
              if man.flow.is_cur_bottom flow then
                None
              else
                let exp = mk_py_call (mk_py_addr_attr cls "__bool__" range) [arg] range in
                man.eval ctx exp flow |>
                eval_compose (fun exp flow ->
                    match etyp exp with
                    | T_bool -> oeval_singleton (Some exp, flow, [])
                    | T_any -> assert false
                    | _ ->
                      let flow = man.exec ctx
                          (Utils.mk_builtin_raise "TypeError" range)
                          flow
                      in
                      oeval_singleton (None, flow, [])
                  )

            in

            let no_bool_flow = man.exec ctx (mk_assume (mk_not (Utils.mk_builtin_call "hasattr" [arg; mk_string "__bool__" range] range) range) range) flow in

            let len_case =
              let flow = man.exec ctx (mk_assume (Utils.mk_builtin_call "hasattr" [arg; mk_string "__len__" range] range) range) no_bool_flow in
              if man.flow.is_cur_bottom flow then
                None
              else
                let can_be_true =
                  not @@ man.flow.is_cur_bottom @@
                  man.exec ctx
                    (mk_assume
                       (mk_binop
                          (mk_py_call (mk_py_addr_attr cls "__len__" range) [arg] range)
                          O_ne
                          (mk_zero range)
                          range
                       )
                       range
                    ) flow
                in
                let can_be_false =
                  not @@ man.flow.is_cur_bottom @@
                  man.exec ctx
                    (mk_assume
                       (mk_binop
                          (mk_py_call (mk_py_addr_attr cls "__len__" range) [arg] range)
                          O_eq
                          (mk_zero range)
                          range
                       )
                       range
                    ) flow
                in
                match can_be_true, can_be_false with
                | true, false -> oeval_singleton (Some (mk_true range), flow, [])
                | false, true -> oeval_singleton (Some (mk_false range), flow, [])
                | true, true -> oeval_singleton (Some (mk_int_interval 0 1 range), flow, [])
                | false, false -> oeval_singleton (None, flow, [])
            in

            let default_case =
              let flow = man.exec ctx (mk_assume (mk_not (Utils.mk_hasattr arg "__len__" range) range) range) no_bool_flow in
              if man.flow.is_cur_bottom flow then
                None
              else
                oeval_singleton (Some (mk_true range), flow, [])
            in

            oeval_join bool_case len_case |> oeval_join default_case

        )

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "bool.__new__")}}, _, []) ->
      let flow = man.exec ctx
          (Utils.mk_builtin_raise "TypeError" range)
          flow
      in
      oeval_singleton (None, flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "bool.__bool__")}}, [{etyp = T_bool} as self], []) ->
      oeval_singleton (Some self, flow, [])


    (** Integers *)
    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "int.__new__")}}, [cls], [])
      ->
      oeval_singleton (Some (mk_zero range), flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "int.__new__")}}, [cls; {etyp = T_int} as arg], [])
      ->
      oeval_singleton (Some arg, flow, [])


    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "int.__new__")}}, [cls; {etyp = T_bool} as arg], [])
      ->
      re_eval_singleton (man.eval ctx) (Some (mk_binop arg O_py_floor_div (mk_one range) range), flow, [])


    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "int.__new__")}}, [cls; {etyp = T_float}], [])
      ->
      Framework.Exceptions.panic "int(float) not supported"


    | E_py_call(({ekind = E_addr {addr_kind = A_py_function (F_builtin "int.__new__")}} as f), [cls; ({etyp = T_string} as arg)], []) ->
      eval man ctx {exp with ekind = E_py_call(f, [cls; arg; mk_int 10 range], [])} flow

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "int.__new__")}}, [cls; ({etyp = T_string} as arg); base], [])
    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "int.__new__")}}, [cls; ({etyp = T_string} as arg)], [(Some "base", base)])
      ->
      begin
        debug "int with base";
        match etyp base with
        | T_int ->
          begin
            let s = man.ask ctx (Memory.Nonrel.Domain.QEval arg) flow |> Option.none_to_exn in
            let base = man.ask ctx (Memory.Nonrel.Domain.QEval base) flow |> Option.none_to_exn in
            debug "base = %a" Memory.Value.print base;
            let tmp = mktmp ~vtyp:T_int () in
            let n = mk_var tmp range in
            try
              let low, up = Memory.Value.(I.get_bounds base.int) in
              if Z.(geq (up - low) (of_int 5)) then
                oeval_singleton (Some (mk_top T_int range), flow, [])
              else
              Memory.Value.(S.fold (fun s acc ->
                  let s = String.trim s in
                  I.fold (fun acc base ->
                      debug "base = %a" Z.pp_print base;
                      try
                        if Z.equal base Z.zero then
                          let flow = man.exec ctx (mk_assign n (mk_z (Z.of_string s) range) range) flow in
                          oeval_singleton (Some n, flow, [mk_remove_var tmp range]) |>
                          oeval_join acc
                        else if Z.equal base Z.one || Z.lt base Z.zero || Z.gt base (Z.of_int 32)  then
                          let flow = man.exec ctx
                              (Utils.mk_builtin_raise "ValueError" range)
                              flow
                          in
                          oeval_singleton (None, flow, []) |>
                          oeval_join acc
                        else if Z.gt base (Z.of_int 16) then
                          Framework.Exceptions.panic "base > 16 not supported"
                        else
                          let s =
                            if List.mem (String.sub s 0 2) ["0b"; "0B"; "0o"; "0O"; "0x"; "0X"] then
                              String.sub s 2 (String.length s - 2)
                            else
                              s
                          in
                          let flow = man.exec ctx (mk_assign n (mk_z (Z.of_string_base (Z.to_int base) s) range) range) flow in
                          oeval_singleton (Some n, flow, [mk_remove_var tmp range]) |>
                          oeval_join acc
                      with Invalid_argument _ ->
                        let flow = man.exec ctx
                            (Utils.mk_builtin_raise "ValueError" range)
                            flow
                        in
                        oeval_singleton (None, flow, []) |>
                        oeval_join acc
                    ) acc base.int
                ) s.string None)

            with Top.Found_TOP ->
              Framework.Exceptions.panic "TODO: fail with a top interval in int.__new__"

          end

        | T_addr ->
          Framework.Exceptions.panic "Instance as base not supported in int conversion"

        | _ ->
          let flow = man.exec ctx
              (Utils.mk_builtin_raise "TypeError" range)
              flow
          in
          oeval_singleton (None, flow, [])


      end


    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "int.__bool__")}}, [{etyp = T_int | T_bool} as self], [])
    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "float.__bool__")}}, [{etyp = T_float} as self], [])
      ->
      begin
        let can_be_true =
          not @@ man.flow.is_cur_bottom @@
          man.exec ctx (mk_assume (mk_binop self O_ne (mk_zero range) range) range) flow
        in

        let can_be_false =
          not @@ man.flow.is_cur_bottom @@
          man.exec ctx (mk_assume (mk_binop self O_eq (mk_zero range) range) range) flow
        in

        match can_be_true, can_be_false with
        | true, false -> oeval_singleton (Some (mk_true range), flow, [])
        | false, true -> oeval_singleton (Some (mk_false range), flow, [])
        | true, true -> oeval_singleton (Some (mk_int_interval 0 1 range), flow, [])
        | false, false -> oeval_singleton (None, flow, [])

      end


    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "int.__bool__")}}, _, [])
    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "float.__bool__")}}, _, [])

      ->
      let flow = man.exec ctx
          (Utils.mk_builtin_raise "TypeError" range)
          flow
      in
      oeval_singleton (None, flow, [])



    (** Floats *)

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "float.__new__")}}, [cls], [])
      ->
      oeval_singleton (Some (mk_float 0. range), flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "float.__new__")}}, [cls; {etyp = T_float} as arg], [])
      ->
      oeval_singleton (Some arg, flow, [])


    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "float.__new__")}}, [cls; {etyp = T_bool | T_int} as arg], [])
      ->
      re_eval_singleton (man.eval ctx) (Some (mk_binop arg (O_div T_any) (mk_one range) range), flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin "float.__new__")}}, [cls; {etyp = T_string} as arg], [])
      ->
      begin
        let s = man.ask ctx (Memory.Nonrel.Domain.QEval arg) flow |> Option.none_to_exn in

        let tmp = mktmp ~vtyp:T_int () in
        let f = mk_var tmp range in
        try
          Memory.Value.(S.fold (fun s acc ->
              let s = String.trim s |> String.lowercase_ascii in
              debug "float(%s)" s;
              try
                let e =
                  if List.mem s ["nan"; "+nan"; "-nan"] then mk_float_interval (-. infinity) (infinity) range else
                  if List.mem s ["inf"; "infinity"; "+inf"; "+infinity"] then mk_float infinity range else
                  if List.mem s ["-inf"; "-infinity"] then mk_float (-. infinity) range
                  else mk_float (float_of_string s) range
                in
                let flow = man.exec ctx (mk_assign f e range) flow in
                oeval_singleton (Some f, flow, [mk_remove_var tmp range]) |>
                oeval_join acc

              with _ ->
                Framework.Exceptions.panic "float(%s) not supported" s
            ) s.string None)

        with Top.Found_TOP ->
          Framework.Exceptions.panic "TODO: fail with a top interval in float.__new__"

      end


    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin f)}}, [e1; e2], [])
      when is_number_binop_function f ->
      let _, f = split_dot_name f |> Option.none_to_exn in
      let op = Operators.fun_to_binop f in
      if is_binop_valid op e1.etyp e2.etyp then
        let t = coerce op e1.etyp e2.etyp in
        let op' = change_operator_type t op in
        let exp' = mk_binop e1 op' e2 ~etyp:t range in
        oeval_singleton (Some exp', flow, [])
      else
        oeval_singleton (Some (mk_py_not_implemented range), flow, [])

    | E_py_call({ekind = E_addr {addr_kind = A_py_function (F_builtin f)}}, [e], [])
      when is_number_unop_function f
      ->
      debug "arithmetic unop";
      let _, f = split_dot_name f |> Option.none_to_exn in
      let op = Operators.fun_to_unop f in
      let t =
        match e.etyp with
        | T_int -> T_int
        | T_float -> T_float
        | T_bool -> T_int
        | _ -> assert false
      in
      let op' = change_operator_type t op in
      let exp' = mk_unop op' e ~etyp:t range in
      oeval_singleton (Some exp', flow, [])


    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin f)})}, _, _)
      when is_number_function f ->
      Framework.Exceptions.panic "function %s not implemented" f



    | _ -> None


  let init man ctx prog flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain)
