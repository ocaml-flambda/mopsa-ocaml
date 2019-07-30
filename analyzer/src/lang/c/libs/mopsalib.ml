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


  let is_rand_function = function
    | "_mopsa_rand_s8"
    | "_mopsa_rand_u8"
    | "_mopsa_rand_s16"
    | "_mopsa_rand_u16"
    | "_mopsa_rand_s32"
    | "_mopsa_rand_u32"
    | "_mopsa_rand_s64"
    | "_mopsa_rand_u64"
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
    | "_mopsa_rand_s64" -> s64
    | "_mopsa_rand_u64" -> u64
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
    | "_mopsa_range_s64"
    | "_mopsa_range_u64"
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
    | "_mopsa_range_s64" -> s64
    | "_mopsa_range_u64" -> u64
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
        (Flow.print man.lattice.print) flow
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
      let stmt = mk_satisfy cond exp.erange in
      let flow = man.exec stmt flow in
      Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
      Option.return


    | E_c_builtin_call("_mopsa_assert_safe", []) ->
      let is_safe = Flow.get_alarms flow |> AlarmSet.is_empty in
      let flow =
        if is_safe
        then flow
        else Universal.Iterators.Unittest.raise_assert_fail exp exp.erange man flow
      in
      Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
      Option.return

    | E_c_builtin_call("_mopsa_assert_unsafe", []) ->
      let is_safe = Flow.get_alarms flow |> AlarmSet.is_empty in
      let flow =
        if not is_safe
        then Flow.remove_alarms flow
        else Universal.Iterators.Unittest.raise_assert_fail exp exp.erange man flow
      in
      Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
      Option.return


    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
