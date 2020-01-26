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
open Zone
open Universal.Zone
open Common.Points_to
open Common.Base
open Universal.Numeric.Common


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
      provides = [Z_c, Z_c_low_level];
      uses = [
        Z_c, Z_u_num;
        Z_c, Z_c_points_to
      ]
    }
  }

  let alarms = [Universal.Iterators.Unittest.A_assert_fail_cls]

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



  (** {2 Printing of abstract values} *)
  (** ******************************* *)

  let exp_to_str exp =
    let () = pp_expr Format.str_formatter exp in
    Format.flush_str_formatter ()


  (** Print the value of an integer expression *)
  let print_int_value exp ?(display=exp_to_str exp) man fmt flow =
    let evl = man.eval ~zone:(Z_c,Z_u_num) exp flow in
    Format.fprintf fmt "%s = %a"
      display
      (Cases.print_some (fun fmt e flow ->
           let itv = man.ask (mk_int_interval_query e) flow in
           pp_int_interval fmt itv
         )
      ) evl

  (** Print the value of a float expression *)
  let print_float_value exp ?(display=exp_to_str exp) man fmt flow =
    let evl = man.eval ~zone:(Z_c,Z_u_num) exp flow in
    Format.fprintf fmt "%s = %a"
      display
      (Cases.print_some (fun fmt e flow ->
           let itv = man.ask (mk_float_interval_query e) flow in
           pp_float_interval fmt itv
         )
      ) evl


  (** Print the value of a pointer expression *)
  let print_pointer_value exp ?(display=exp_to_str exp) man fmt flow =
    let evl = man.eval ~zone:(Z_c,Z_c_points_to) exp flow in
    Format.fprintf fmt "%s ⇝ %a"
      display
      (Cases.print_some (fun fmt e flow ->
           match ekind e with
           | E_c_points_to (P_block(base,offset)) ->
             let evl = man.eval ~zone:(Z_c_scalar,Z_u_num) offset flow in
             Format.fprintf fmt "&(%a%a)"
               pp_base base
               (Cases.print_some (fun fmt e flow ->
                    let itv = man.ask (Universal.Numeric.Common.mk_int_interval_query e) flow in
                    Universal.Numeric.Values.Intervals.Integer.Value.print fmt itv
                  )
               ) evl

           | E_c_points_to p -> pp_points_to fmt p
           | _ -> assert false
         )
      ) evl


  (** Print the values of an array *)
  let rec print_array_values exp ?(display=exp_to_str exp) man fmt flow =
    match remove_casts exp |> etyp |> remove_typedef_qual with
    | T_c_array(_, C_array_length_cst len) ->
      (* Create an interval expression [0, len - 1] to use as an index *)
      let itv = mk_z_interval Z.zero (Z.pred len) exp.erange in
      let exp' = mk_c_subscript_access exp itv exp.erange in
      let display =
        let () = Format.fprintf Format.str_formatter "%s%a" display pp_expr itv in
        Format.flush_str_formatter ()
      in
      print_value exp' ~display man fmt flow
    | _ ->
      warn_at exp.erange "_mopsa_print: unsupported array type %a" pp_typ exp.etyp


  (** Print fields values of a record *)
  and print_record_values exp ?(display=exp_to_str exp) man fmt flow =
    let fields = match remove_typedef_qual exp.etyp with
      | T_c_record {c_record_fields} -> c_record_fields
      | _ -> assert false
    in
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
      (fun fmt field ->
         let exp' = mk_c_member_access exp field exp.erange in
         let display =
           let () = Format.fprintf Format.str_formatter "%s.%s" display field.c_field_org_name in
           Format.flush_str_formatter ()
         in
         print_value exp' ~display man fmt flow
      ) fmt fields


  (** Print the value of an expression *)
  and print_value exp ?(display=exp_to_str exp) man fmt flow =
    if is_c_int_type exp.etyp then print_int_value exp ~display man fmt flow
    else if is_c_float_type exp.etyp then print_float_value exp ~display man fmt flow
    else if is_c_record_type exp.etyp then print_record_values exp ~display man fmt flow
    else if is_c_array_type @@ etyp @@ remove_casts exp then print_array_values exp ~display man fmt flow
    else if is_c_pointer_type exp.etyp then print_pointer_value exp ~display man fmt flow
    else warn_at exp.erange "_mopsa_print: unsupported type %a" pp_typ exp.etyp


  (** Print the values of a list of expressions *)
  let print_values args man fmt flow =
    Format.fprintf fmt "@[<v>%a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt e -> print_value e man fmt flow)
      ) args


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
       Framework.Output.Factory.print (erange exp) (Flow.print man.lattice.print) flow;
       Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
       Option.return

    | E_c_builtin_call("_mopsa_print", args) ->
      Framework.Output.Factory.print (erange exp) (print_values args man) flow;
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
        else Universal.Iterators.Unittest.raise_assert_fail exp exp.erange man ~force:true flow
      in
      Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
      Option.return

    | E_c_builtin_call("_mopsa_assert_unsafe", []) ->
      let is_safe = Flow.get_alarms flow |> AlarmSet.is_empty in
      let flow =
        if not is_safe
        then Flow.remove_alarms flow
        else Universal.Iterators.Unittest.raise_assert_fail exp exp.erange ~force:true man flow
      in
      Eval.singleton (mk_int 0 ~typ:u8 exp.erange) flow |>
      Option.return


    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
