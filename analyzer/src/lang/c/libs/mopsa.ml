(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of C intra-procedural control flow *)

open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Lattice
open Framework.Domains.Stateless
open Framework.Utils
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.libs.mopsa"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let is_builtin_function = function
    | "_mopsa_rand_int"
    | "_mopsa_panic"
    | "_mopsa_assert_true"
    | "_mopsa_assert_false"
    | "_mopsa_assert_safe"
    | "_mopsa_assert_unsafe"
    | "_mopsa_assert_error"
    | "_mopsa_assert_error_at_line" -> true
    | _ -> false

  let error_token_to_code = function
    | Alarms.TOutOfBound _ -> 1
    | Alarms.TNullDeref _ -> 2
    | Alarms.TInvalidDeref _ -> 3
    | _ -> assert false

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prog man ctx flow = ctx, flow

  let exec stmt man ctx flow = None

  let eval exp man ctx flow =
    match ekind exp with
    | E_c_function(f) when is_builtin_function f.c_func_var.vname ->
      debug "builtin function";
      let exp' = mk_expr (E_c_builtin_function f.c_func_var.vname) ~etyp:T_c_builtin_fn exp.erange in
      oeval_singleton (Some exp', flow, [])

    | E_c_call({ekind = E_c_builtin_function "_mopsa_rand_int"}, [a; b]) ->
      let erange = exp.erange in
      let typ = T_c_integer(C_signed_long) in
      let tmp = mktmp ~vtyp:typ () in
      let v = mk_var tmp erange in
      let flow = man.exec (mk_assume (
          mk_binop
            (mk_binop a O_le v (tag_range erange "in1") ~etyp:typ)
            O_log_and
            (mk_binop v O_le b (tag_range erange "in2") ~etyp:typ)
            erange
        ) erange) ctx flow in
      re_eval_singleton (Some v, flow, [mk_remove_var tmp erange]) man ctx

    | E_c_call({ekind = E_c_builtin_function "_mopsa_panic"}, [msg]) ->
      let rec remove_cast e =
        match ekind e with
        | E_c_cast(e', _) -> remove_cast e'
        | _ -> e
      in
      let s = match remove_cast msg |> ekind with
        | E_constant(C_c_string (s, _)) -> s
        | _ -> assert false
      in
      Framework.Exceptions.panic "%s" s

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_true"}, [cond]) ->
      let stmt = mk_assert cond exp.erange in
      let flow = man.exec stmt ctx flow in
      oeval_singleton (Some (mk_int 0 exp.erange), flow, [])

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_false"}, [cond]) ->
      let stmt = mk_assert (mk_not cond exp.erange) exp.erange in
      let flow = man.exec stmt ctx flow in
      oeval_singleton (Some (mk_int 0 exp.erange), flow, [])

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_safe"}, []) ->
      begin
        let error_env = man.flow.fold (fun acc env -> function
            | tk when Alarms.is_error_token tk -> man.env.join acc env
            | _ -> acc
          ) man.env.bottom flow in
        let exception BottomFound in
        try
          let cond =
            match man.flow.is_cur_bottom flow, man.env.is_bottom error_env with
            | false, true -> mk_one
            | true, false -> mk_zero
            | false, false -> mk_int_interval 0 1
            | true, true -> raise BottomFound
          in
          let stmt = mk_assert (cond exp.erange) exp.erange in
          let cur = man.flow.get TCur flow in
          let flow = man.flow.set TCur man.env.top flow |>
                     man.exec stmt ctx |>
                     man.flow.set TCur cur
          in
          oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
        with BottomFound ->
          oeval_singleton (None, flow, [])
      end

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_unsafe"}, []) ->
      begin
        let error_env = man.flow.fold (fun acc env -> function
            | tk when Alarms.is_error_token tk -> man.env.join acc env
            | _ -> acc
          ) man.env.bottom flow in
        let cond =
          match man.flow.is_cur_bottom flow, man.env.is_bottom error_env with
          | false, true -> mk_zero
          | true, false -> mk_one
          | false, false -> mk_int_interval 0 1
          | true, true -> mk_zero
        in
        let stmt = mk_assert (cond exp.erange) exp.erange in
        let cur = man.flow.get TCur flow in
        let flow = man.flow.set TCur man.env.top flow in
        let flow = man.exec stmt ctx flow |>
                   man.flow.filter (fun _ -> function tk when Alarms.is_error_token tk -> false | _ -> true) |>
                   man.flow.set TCur cur
        in
        oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
      end

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_error"}, [{ekind = E_constant(C_int code)}]) ->
      begin
        let code = Z.to_int code in
        let this_error_env = man.flow.fold (fun acc env -> function
            | tk when Alarms.is_error_token tk && code = error_token_to_code tk -> man.env.join acc env
            | _ -> acc
          ) man.env.bottom flow in
        let cond =
          match man.flow.is_cur_bottom flow, man.env.is_bottom this_error_env with
          | true, false -> mk_one
          | _, true -> mk_zero
          | false, false ->  mk_int_interval 0 1
        in
        let stmt = mk_assert (cond exp.erange) exp.erange in
        let cur = man.flow.get TCur flow in
        let flow = man.flow.set TCur man.env.top flow in
        let flow = man.exec stmt ctx flow |>
                   man.flow.filter (fun _ -> function tk when Alarms.is_error_token tk && code = error_token_to_code tk -> false | _ -> true) |>
                   man.flow.set TCur cur
        in
        oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
      end

    | E_c_call({ekind = E_c_builtin_function "_mopsa_assert_error_at_line"}, [{ekind = E_constant(C_int code)}; {ekind = E_constant(C_int line)}]) ->
            begin
        let code = Z.to_int code and line = Z.to_int line in
        let this_error_env = man.flow.fold (fun acc env -> function
            | tk when Alarms.is_error_token tk &&
                      code = error_token_to_code tk &&
                      line = (let r = Alarms.error_token_range tk |> get_origin_range in r.range_begin.loc_line) ->
              man.env.join acc env
            | _ -> acc
          ) man.env.bottom flow in
        let cond =
          match man.flow.is_cur_bottom flow, man.env.is_bottom this_error_env with
          | true, false -> mk_one
          | _, true -> mk_zero
          | false, false ->  mk_int_interval 0 1
        in
        let stmt = mk_assert (cond exp.erange) exp.erange in
        let cur = man.flow.get TCur flow in
        let flow = man.flow.set TCur man.env.top flow in
        let flow = man.exec stmt ctx flow |>
                   man.flow.filter (fun _ -> function
                       | tk when Alarms.is_error_token tk &&
                                 code = error_token_to_code tk &&
                                 line = (let r = Alarms.error_token_range tk |> get_origin_range in r.range_begin.loc_line) -> false
                       | _ -> true) |>
                   man.flow.set TCur cur
        in
        oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
      end

    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain)
