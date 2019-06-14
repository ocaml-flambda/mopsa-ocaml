(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Evaluation of MOPSA built-in functions *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Universal.Ast
open Ast



module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.libs.mopsalib"
    end)

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [];
      uses = []
    };

    ieval = {
      provides = [Zone.Z_c, Zone.Z_c_low_level];
      uses = []
    }
  }

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

  let is_rand_function = function
    | "_mopsa_rand_s8"
    | "_mopsa_rand_u8"
    | "_mopsa_rand_s16"
    | "_mopsa_rand_u16"
    | "_mopsa_rand_s32"
    | "_mopsa_rand_u32"
    | "_mopsa_rand_float"
    | "_mopsa_rand_double"
    | "_mopsa_rand_void_pointer"
      -> true

    | _ -> false

  let extract_rand_type = function
    | "_mopsa_rand_s8" -> s8
    | "_mopsa_rand_u8" -> u8
    | "_mopsa_rand_s16" -> s16
    | "_mopsa_rand_u16" -> u16
    | "_mopsa_rand_s32" -> s32
    | "_mopsa_rand_u32" -> u32
    | "_mopsa_rand_float" -> T_c_float C_float
    | "_mopsa_rand_double" -> T_c_float C_double
    | "_mopsa_rand_void_pointer" -> T_c_pointer T_c_void
    | f -> panic "extract_rand_type: invalid argument %s" f


  let mk_rand typ range man flow =
    Eval.singleton (mk_top typ range) flow


  let is_range_function = function
    | "_mopsa_range_s8"
    | "_mopsa_range_u8"
    | "_mopsa_range_s16"
    | "_mopsa_range_u16"
    | "_mopsa_range_s32"
    | "_mopsa_range_u32"
    | "_mopsa_range_int"
    | "_mopsa_range_float"
    | "_mopsa_range_double"
      -> true

    | _ -> false

  let extract_range_type = function
    | "_mopsa_range_s8" -> s8
    | "_mopsa_range_u8" -> u8
    | "_mopsa_range_s16" -> s16
    | "_mopsa_range_u16" -> u16
    | "_mopsa_range_s32" -> s32
    | "_mopsa_range_u32" -> u32
    | "_mopsa_range_float" -> T_c_float C_float
    | "_mopsa_range_double" -> T_c_float C_double
    | f -> panic "extract_range_type: invalid argument %s" f


  let mk_range typ l u range man flow =
    let tmp = mktmp ~typ () in
    let v = mk_var tmp range in
    let flow = man.exec (mk_block [
        mk_add_var tmp range;
        mk_assume (mk_in v l u range) range
      ] range) flow
    in
    Eval.singleton v flow ~cleaners:[mk_remove_var tmp range]



  (*==========================================================================*)
  (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init _ _ flow =  flow

  let exec zone stmt man flow = None

  let eval zone exp man flow =
    match ekind exp with
    | E_c_builtin_call(f, []) when is_rand_function f ->
      mk_rand (extract_rand_type f) exp.erange man flow |>
      Option.return

    | E_c_builtin_call(f, [l;u]) when is_range_function f ->
      mk_range (extract_range_type f) l u exp.erange man flow |>
      Option.return

    | E_c_builtin_call("_mopsa_invalid_pointer", []) ->
      Eval.singleton (mk_c_invalid_pointer exp.erange) flow |>
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
      Exceptions.panic "%s" s

    | E_c_builtin_call("_mopsa_print", []) ->
        Debug.debug ~channel:"print" "%a@\n  @[%a@]"
        pp_position (erange exp |> get_range_start)
        (Flow.print man.lattice) flow
      ;
      Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
      Option.return


    | E_c_builtin_call("_mopsa_assume", [cond]) ->
      let stmt = mk_assume cond exp.erange in
      let flow = man.exec stmt flow in
      Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
      Option.return


    | E_c_builtin_call("_mopsa_assert", [cond]) ->
      let stmt = mk_assert cond exp.erange in
      let flow = man.exec stmt flow in
      Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
      Option.return

    | E_c_builtin_call("_mopsa_assert_exists", [cond]) ->
      let flow' = man.exec (mk_assume cond exp.erange) flow in
      let cond' =
        if man.lattice.is_bottom (Flow.get T_cur man.lattice flow') then
          mk_zero exp.erange
        else
          mk_one exp.erange
      in
      let flow = man.exec (mk_assert cond' exp.erange) flow in
      Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
      Option.return

    | E_c_builtin_call("_mopsa_assert_false", [cond]) ->
      assert false

    | E_c_builtin_call("_mopsa_assert_safe", []) ->
      begin
        let ctx = Flow.get_unit_ctx flow in
        let error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm _ -> man.lattice.join ctx acc env
            | _ -> acc
          ) man.lattice.bottom flow in
        let exception BottomFound in
        try
          let cond =
            match Flow.get T_cur man.lattice flow |> man.lattice.is_bottom,
                  man.lattice.is_bottom error_env
            with
            | false, true -> mk_one
            | true, false -> mk_zero
            | false, false -> mk_int_interval 0 1
            | true, true -> raise BottomFound
          in
          let stmt = mk_assert (cond ~typ:u8 exp.erange) exp.erange in
          let cur = Flow.get T_cur man.lattice flow in
          let flow = Flow.set T_cur man.lattice.top man.lattice flow |>
                     man.exec stmt |>
                     Flow.set T_cur cur man.lattice
          in
          Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
          Option.return
        with BottomFound ->
          Eval.empty_singleton flow |>
          Option.return
      end

     | E_c_builtin_call("_mopsa_assert_unsafe", []) ->
      begin
        let ctx = Flow.get_unit_ctx flow in
        let error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm _ -> man.lattice.join ctx acc env
            | _ -> acc
          ) man.lattice.bottom flow in
        let cond =
          match Flow.get T_cur man.lattice flow |> man.lattice.is_bottom,
                man.lattice.is_bottom error_env
          with
          | false, true -> mk_zero
          | true, false -> mk_one
          | false, false -> mk_int_interval 0 1
          | true, true -> mk_zero
        in
        let stmt = mk_assert (cond ~typ:u8 exp.erange) exp.erange in
        let flow1 = Flow.set T_cur man.lattice.top man.lattice flow in
        let flow2 = man.exec stmt flow1 in
        (* Since the unsafe here is "normal", so we remove all alarms *)
        let flow3 = Flow.filter (fun tk _ ->
            match tk with
            | T_alarm _ -> false
            | _ -> true
          ) flow2
        in
        Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow3 |>
        Option.return
      end

    | E_c_builtin_call("_mopsa_assert_error", [{ekind = E_constant(C_int code)}]) ->
      begin
        let code = Z.to_int code in
        let ctx = Flow.get_unit_ctx flow in
        let this_error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm a when is_c_alarm a &&
                             code = alarm_to_code a
              ->
              man.lattice.join ctx acc env
            | _ -> acc
          ) man.lattice.bottom flow in
        let cond =
          match Flow.get T_cur man.lattice flow |> man.lattice.is_bottom,
                man.lattice.is_bottom this_error_env
          with
          | true, false -> mk_one
          | _, true -> mk_zero
          | false, false ->  mk_int_interval 0 1
        in
        let stmt = mk_assert (cond ~typ:u8 exp.erange) exp.erange in
        let cur = Flow.get T_cur man.lattice flow in
        let flow = Flow.set T_cur man.lattice.top man.lattice flow in
        let flow = man.exec stmt flow |>
                   Flow.filter (fun tk _ ->
                       match tk with
                       | T_alarm a when is_c_alarm a &&
                                        code = alarm_to_code a
                         -> false
                       | _ -> true
                     ) |>
                   Flow.set T_cur cur man.lattice
        in
        Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
        Option.return
      end

    | E_c_builtin_call("_mopsa_assert_error_at_line", [{ekind = E_constant(C_int code)}; {ekind = E_constant(C_int line)}]) ->
      begin
        let code = Z.to_int code and line = Z.to_int line in
        let ctx = Flow.get_unit_ctx flow in
        let this_error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm a
              when is_c_alarm a
                && code = alarm_to_code a
                && line = get_range_line @@ fst a.alarm_trace
              ->
              man.lattice.join ctx acc env
            | _ -> acc
          ) man.lattice.bottom flow in
        let cond =
          match Flow.get T_cur man.lattice flow |> man.lattice.is_bottom,
                man.lattice.is_bottom this_error_env
          with
          | true, false -> mk_one
          | _, true -> mk_zero
          | false, false ->  mk_int_interval 0 1
        in
        let stmt = mk_assert (cond ~typ:u8 exp.erange) exp.erange in
        let cur = Flow.get T_cur man.lattice flow in
        let flow = Flow.set T_cur man.lattice.top man.lattice flow in
        let flow = man.exec stmt flow |>
                   Flow.filter (fun tk _ ->
                       match tk with
                       | T_alarm a
                         when is_c_alarm a
                           && code = alarm_to_code a
                           && line = get_range_line @@ fst a.alarm_trace
                         -> false
                       | _ -> true) |>
                   Flow.set T_cur cur man.lattice
        in
        Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
        Option.return
      end

    | E_c_builtin_call("_mopsa_assert_error_exists", [{ekind = E_constant(C_int code)}]) ->
      begin
        let code = Z.to_int code in
        let ctx = Flow.get_unit_ctx flow in
        let error_env = Flow.fold (fun acc tk env ->
            match tk with
            | T_alarm a
              when is_c_alarm a
                && code = alarm_to_code a
              -> man.lattice.join ctx acc env
            | _ -> acc
          ) man.lattice.bottom flow in
        let cur = Flow.get T_cur man.lattice flow in
        let cur' =
          if man.lattice.is_bottom cur
          then man.lattice.top
          else cur
        in
        let cond =
          if man.lattice.is_bottom error_env
          then mk_zero ~typ:u8 exp.erange
          else mk_one ~typ:u8 exp.erange
        in
        let stmt = mk_assert cond exp.erange in
        let flow' = Flow.set T_cur cur' man.lattice flow |>
                    man.exec stmt |>
                    Flow.filter (fun tk _ ->
                        match tk with
                        | T_alarm a when is_c_alarm a &&
                                         code = alarm_to_code a
                          -> false
                        | _ -> true
                      ) |>
                    Flow.set T_cur cur man.lattice
        in
        Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow' |>
        Option.return
      end

    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
