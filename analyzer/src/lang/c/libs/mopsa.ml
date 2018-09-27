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
  let eval_interface = {
    export = [
      Zone.Z_c, Zone.Z_c_scalar;
      Zone.Z_c_scalar, Zone.Z_c_points_to_fun;
    ]; import = []}

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

  let is_c_alarm a =
    match a.alarm_kind with
    | Alarms.AOutOfBound
    | Alarms.ANullDeref
    | Alarms.AInvalidDeref
    | Alarms.AIntegerOverflow
    | Alarms.ADivideByZero -> true
    | _ -> false

  let alarm_to_code a =
    match a.alarm_kind with
    | Alarms.AOutOfBound -> 1
    | Alarms.ANullDeref -> 2
    | Alarms.AInvalidDeref -> 3
    | Alarms.AIntegerOverflow -> 4
    | Alarms.ADivideByZero -> 5
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
    | E_c_function(f)
      when is_builtin_function f.c_func_var.vname
        && sat_zone Zone.Z_c_points_to_fun (snd zone)
      ->
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

    | E_c_builtin_call("_mopsa_range_unsigned_char", []) ->
      rand_int Ast.C_unsigned_char exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_range_int", []) ->
      rand_int Ast.C_signed_int exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_range_unsigned_int", []) ->
      rand_int Ast.C_unsigned_int exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_range_short", []) ->
      rand_int Ast.C_signed_short exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_range_unsigned_short", []) ->
      rand_int Ast.C_unsigned_short exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_range_long", []) ->
      rand_int Ast.C_signed_long exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_range_unsigned_long", []) ->
      rand_int Ast.C_unsigned_long exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_range_long_long", []) ->
      rand_int Ast.C_signed_long_long exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_range_unsigned_long_long", []) ->
      rand_int Ast.C_unsigned_long_long exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_rand_int", [a; b]) ->
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

    | E_c_builtin_call("_mopsa_rand_unsigned_long", [a; b]) ->
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

    | E_c_builtin_call("_mopsa_panic", [msg]) ->
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

    | E_c_builtin_call("_mopsa_print", []) ->
        Debug.debug ~channel:"print" "%a@\n  @[%a@]"
        pp_range_verbose (erange exp)
        (Flow.print man) flow
      ;
      Eval.singleton (mk_int 0 exp.erange) flow |>
      Option.return


    | E_c_builtin_call("_mopsa_assert_true", [cond]) ->
      let stmt = mk_assert cond exp.erange in
      let flow = man.exec stmt flow in
      Eval.singleton (mk_int 0 exp.erange) flow |>
      Option.return

    | E_c_builtin_call("_mopsa_assert_exists", [cond]) ->
      let stmt = {skind = S_simple_assert(cond,false,true); srange = exp.erange} in
      let flow = man.exec stmt flow in
      Eval.singleton (mk_int 0 exp.erange) flow |>
      Option.return

    | E_c_builtin_call("_mopsa_assert_false", [cond]) ->
      assert false

    | E_c_builtin_call("_mopsa_assert_safe", []) ->
      begin
        let annot = Flow.get_all_annot flow in
        let error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm _ -> man.join annot acc env
            | _ -> acc
          ) man.bottom man flow in
        let exception BottomFound in
        try
          let cond =
            match Flow.get T_cur man flow |> man.is_bottom,
                  man.is_bottom error_env
            with
            | false, true -> mk_one
            | true, false -> mk_zero
            | false, false -> mk_int_interval 0 1
            | true, true -> raise BottomFound
          in
          let stmt = mk_assert (cond exp.erange) exp.erange in
          let cur = Flow.get T_cur man flow in
          let flow = Flow.set T_cur man.top man flow |>
                     man.exec stmt |>
                     Flow.set T_cur cur man
          in
          Eval.singleton (mk_int 0 exp.erange) flow |>
          Option.return
        with BottomFound ->
          Eval.empty_singleton flow |>
          Option.return
      end

     | E_c_builtin_call("_mopsa_assert_unsafe", []) ->
      begin
        let annot = Flow.get_all_annot flow in
        let error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm _ -> man.join annot acc env
            | _ -> acc
          ) man.bottom man flow in
        let cond =
          match Flow.get T_cur man flow |> man.is_bottom,
                man.is_bottom error_env
          with
          | false, true -> mk_zero
          | true, false -> mk_one
          | false, false -> mk_int_interval 0 1
          | true, true -> mk_zero
        in
        let stmt = mk_assert (cond exp.erange) exp.erange in
        let cur = Flow.get T_cur man flow in
        let flow = Flow.set T_cur man.top man flow in
        let flow = man.exec stmt flow |>
                   Flow.filter (fun tk _ -> match tk with T_alarm _ -> false | _ -> true) man |>
                   Flow.set T_cur cur man
        in
        Eval.singleton (mk_int 0 exp.erange) flow |>
        Option.return
      end

    | E_c_builtin_call("_mopsa_assert_error", [{ekind = E_constant(C_int code)}]) ->
      begin
        let code = Z.to_int code in
        let annot = Flow.get_all_annot flow in
        let this_error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm a when is_c_alarm a && code = alarm_to_code a -> man.join annot acc env
            | _ -> acc
          ) man.bottom man flow in
        let cond =
          match Flow.get T_cur man flow |> man.is_bottom,
                man.is_bottom this_error_env
          with
          | true, false -> mk_one
          | _, true -> mk_zero
          | false, false ->  mk_int_interval 0 1
        in
        let stmt = mk_assert (cond exp.erange) exp.erange in
        let cur = Flow.get T_cur man flow in
        let flow = Flow.set T_cur man.top man flow in
        let flow = man.exec stmt flow |>
                   Flow.filter (fun tk _ -> match tk with T_alarm a when is_c_alarm a && code = alarm_to_code a -> false | _ -> true) man |>
                   Flow.set T_cur cur man
        in
        Eval.singleton (mk_int 0 exp.erange) flow |>
        Option.return
      end

    | E_c_builtin_call("_mopsa_assert_error_at_line", [{ekind = E_constant(C_int code)}; {ekind = E_constant(C_int line)}]) ->
      begin
        let code = Z.to_int code and line = Z.to_int line in
        let annot = Flow.get_all_annot flow in
        let this_error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm a
              when is_c_alarm a
                && code = alarm_to_code a
                && line = (let r = get_origin_range @@ List.hd a.alarm_trace in r.range_begin.loc_line)
              ->
              man.join annot acc env
            | _ -> acc
          ) man.bottom man flow in
        let cond =
          match Flow.get T_cur man flow |> man.is_bottom,
                man.is_bottom this_error_env
          with
          | true, false -> mk_one
          | _, true -> mk_zero
          | false, false ->  mk_int_interval 0 1
        in
        let stmt = mk_assert (cond exp.erange) exp.erange in
        let cur = Flow.get T_cur man flow in
        let flow = Flow.set T_cur man.top man flow in
        let flow = man.exec stmt flow |>
                   Flow.filter (fun tk _ ->
                       match tk with
                       | T_alarm a
                         when is_c_alarm a
                           && code = alarm_to_code a
                           && line = (let r = get_origin_range @@ List.hd a.alarm_trace in r.range_begin.loc_line)
                         -> false
                       | _ -> true) man |>
                   Flow.set T_cur cur man
        in
        Eval.singleton (mk_int 0 exp.erange) flow |>
        Option.return
      end

    | E_c_builtin_call("_mopsa_assert_error_exists", [{ekind = E_constant(C_int code)}]) ->
      begin
        let code = Z.to_int code in
        let annot = Flow.get_all_annot flow in
        let error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm a
              when is_c_alarm a
                && code = alarm_to_code a
              -> man.join annot acc env
            | _ -> acc
          ) man.bottom man flow in
        let cur = Flow.get T_cur man flow in
        let cur' = if man.is_bottom cur then man.top else cur in
        let cond = if man.is_bottom error_env then mk_zero exp.erange else mk_one exp.erange in
        let stmt = mk_assert cond exp.erange in
        let flow' = Flow.set T_cur cur' man flow |>
                    man.exec stmt |>
                    Flow.filter (fun tk _ -> match tk with T_alarm a when is_c_alarm a && code = alarm_to_code a -> false | _ -> true) man |>
                    Flow.set T_cur cur man
        in
        Eval.singleton (mk_int 0 exp.erange) flow' |>
        Option.return
      end

    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
