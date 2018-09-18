(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Evaluation of MOPSA built-in functions *)

open Framework.Essentials
open Universal.Ast
open Ast

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_libs_mopsa : unit domain
  let id = D_c_libs_mopsa
  let name = "c.libs.mopsa"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_libs_mopsa -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [Zone.Z_c, Zone.Z_c_scalar]; import = []}

  let is_builtin_function = function
    | "_mopsa_range_char"
    | "_mopsa_range_unsigned_char"
    | "_mopsa_range_short"
    | "_mopsa_range_unsigned_short"
    | "_mopsa_range_int"
    | "_mopsa_range_unsigned_int"
    | "_mopsa_range_long"
    | "_mopsa_range_unsigned_long"
    | "_mopsa_range_long_long"
    | "_mopsa_range_unsigned_long_long"
    | "_mopsa_set_debug_channels"
    | "_mopsa_range"
    | "_mopsa_rand_int"
    | "_mopsa_rand_unsigned_long"
    | "_mopsa_panic"
    | "_mopsa_print"
    | "_mopsa_assert_exists"
    | "_mopsa_assert_true"
    | "_mopsa_assert_false"
    | "_mopsa_assert_safe"
    | "_mopsa_assert_unsafe"
    | "_mopsa_assert_error"
    | "_mopsa_assert_error_exists"
    | "_mopsa_assert_error_at_line" -> true
    | _ -> false

  let error_token_to_code = function
    | Alarms.TOutOfBound _ -> 1
    | Alarms.TNullDeref _ -> 2
    | Alarms.TInvalidDeref _ -> 3
    | Alarms.TIntegerOverflow _ -> 4
    | Alarms.TDivideByZero _ -> 5
    | _ -> assert false

  let rand_int t range man flow =
    let l, r = rangeof (T_c_integer t) in
    let ll, rr = mk_z l (tag_range range "ll"), mk_z r (tag_range range "rr") in
    let exp' = mk_expr (E_c_builtin_call("_mopsa_rand_int", [ll; rr])) range ~etyp:(T_c_integer t) in
    man.eval exp' ~zone:(Zone.Z_c, Zone.Z_c_scalar) flow

  (*==========================================================================*)
  (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prog man flow = None

  let exec zone stmt man flow = None

  let eval zone exp man flow =
    match ekind exp with
    | E_c_function(f) when is_builtin_function f.c_func_var.vname ->
      debug "builtin function";
      let exp' = mk_expr (E_c_builtin_function f.c_func_var.vname) ~etyp:T_c_builtin_fn exp.erange in
      Eval.singleton exp' flow |>
      Option.return

    | E_c_builtin_call("_mopsa_set_debug_channels", [e']) ->
      let channels = match ekind e' with
        | E_constant (C_c_string (s,_))
        | E_c_cast({ ekind = E_constant (C_c_string (s,_))},_) -> s
        | _ -> ""
      in
      let () = Debug.set_channels channels in
      Eval.singleton (mk_int 0 exp.erange) flow |>
      Option.return

    | E_c_builtin_call("_mopsa_range_char", []) ->
      rand_int Ast.C_signed_char exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_range_unsigned_char", []) ->
      rand_int Ast.C_unsigned_char exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_range_int", []) ->
      rand_int Ast.C_signed_int exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_range_unsigned_int", []) ->
      rand_int Ast.C_unsigned_int exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_range_short", []) ->
      rand_int Ast.C_signed_short exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_range_unsigned_short", []) ->
      rand_int Ast.C_unsigned_short exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_range_long", []) ->
      rand_int Ast.C_signed_long exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_range_unsigned_long", []) ->
      rand_int Ast.C_unsigned_long exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_range_long_long", []) ->
      rand_int Ast.C_signed_long_long exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_range_unsigned_long_long", []) ->
      rand_int Ast.C_unsigned_long_long exp.erange man flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_rand_int", [a; b]) ->
      let erange = exp.erange in
      let typ = T_c_integer(C_signed_long) in
      let tmp = mk_tmp ~vtyp:typ () in
      let v = mk_var tmp erange in
      let flow = man.exec (mk_assume (
          mk_binop
            (mk_binop a O_le v (tag_range erange "in1") ~etyp:typ)
            O_log_and
            (mk_binop v O_le b (tag_range erange "in2") ~etyp:typ)
            erange
        ) erange) flow
      in
      Eval.singleton v flow ~cleaners:[mk_remove_var tmp erange] |>
      Option.return

    | E_c_builtin_call( "_mopsa_rand_unsigned_long", [a; b]) ->
      let erange = exp.erange in
      let typ = T_c_integer(C_unsigned_long) in
      let tmp = mk_tmp ~vtyp:typ () in
      let v = mk_var tmp erange in
      let flow = man.exec (mk_assume (
          mk_binop
            (mk_binop a O_le v (tag_range erange "in1") ~etyp:typ)
            O_log_and
            (mk_binop v O_le b (tag_range erange "in2") ~etyp:typ)
            erange
        ) erange) flow in
      Eval.singleton v flow ~cleaners:[mk_remove_var tmp erange] |>
      Option.return

    | E_c_builtin_call( "_mopsa_panic", [msg]) ->
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

    | E_c_builtin_call( "_mopsa_assert_true", [cond]) ->
      let stmt = mk_assert cond exp.erange in
      let flow = man.exec stmt flow in
      Eval.singleton (mk_int 0 exp.erange) flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_assert_exists", [cond]) ->
      let stmt = {skind = S_simple_assert(cond,false,true); srange = exp.erange} in
      let flow = man.exec stmt flow in
      Eval.singleton (mk_int 0 exp.erange) flow |>
      Option.return

    | E_c_builtin_call( "_mopsa_assert_false", [cond]) ->
      assert false

    | E_c_builtin_call( "_mopsa_assert_safe", []) ->
      begin
        let annot = Flow.get_all_annot flow in
        let error_env = Flow.fold (fun acc tk env ->
            if Alarms.is_error_token tk then man.join annot acc env
            else acc
          ) man.bottom man flow in
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
                     man.exec ctx stmt |>
                     man.flow.set TCur cur
          in
          oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
        with BottomFound ->
          oeval_singleton (None, flow, [])
      end

    | E_c_builtin_call( "_mopsa_assert_unsafe"}, []) ->
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
        let flow = man.exec ctx stmt flow |>
                   man.flow.filter (fun _ -> function tk when Alarms.is_error_token tk -> false | _ -> true) |>
                   man.flow.set TCur cur
        in
        oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
      end

    | E_c_builtin_call( "_mopsa_assert_error"}, [{ekind = E_constant(C_int code)}]) ->
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
        let flow = man.exec ctx stmt flow |>
                   man.flow.filter (fun _ -> function tk when Alarms.is_error_token tk && code = error_token_to_code tk -> false | _ -> true) |>
                   man.flow.set TCur cur
        in
        oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
      end

    | E_c_builtin_call( "_mopsa_assert_error_at_line"}, [{ekind = E_constant(C_int code)}; {ekind = E_constant(C_int line)}]) ->
      begin
        let () = debug "toto" in
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
        let flow = man.exec ctx stmt flow |>
                   man.flow.filter (fun _ -> function
                       | tk when Alarms.is_error_token tk &&
                                 code = error_token_to_code tk &&
                                 line = (let r = Alarms.error_token_range tk |> get_origin_range in r.range_begin.loc_line) -> false
                       | _ -> true) |>
                   man.flow.set TCur cur
        in
        oeval_singleton (Some (mk_int 0 exp.erange), flow, [])
      end


    | E_c_builtin_call( "_mopsa_assert_error_exists"}, [{ekind = E_constant(C_int code)}]) ->
      begin
        let code = Z.to_int code in
        let error_env = man.flow.fold (fun acc env -> function
            | tk when Alarms.is_error_token tk && code = error_token_to_code tk -> man.env.join acc env
            | _ -> acc
          ) man.env.bottom flow in
        let cur = man.flow.get TCur flow in
        let cur' = if man.env.is_bottom cur then man.env.top else cur in
        let cond = if man.env.is_bottom error_env then mk_zero exp.erange else mk_one exp.erange in
        let stmt = mk_assert cond exp.erange in
        let flow' = man.flow.set TCur cur' flow |>
                   man.exec ctx stmt |>
                   man.flow.filter (fun _ -> function tk when Alarms.is_error_token tk && code = error_token_to_code tk -> false | _ -> true) |>
                   man.flow.set TCur cur
        in
        oeval_singleton (Some (mk_int 0 exp.erange), flow', [])
      end


    | E_c_call(_) ->
      let () = debug "E_c_call did not go in on %a" Framework.Pp.pp_expr exp in None
    | _ -> None

  let ask _ _ _ _  = None

end

let () =
  Stateless.register_domain name (module Domain)
