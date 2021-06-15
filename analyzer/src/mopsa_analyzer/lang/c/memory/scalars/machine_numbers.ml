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

(** Machine representation of C integers and floats *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Stubs.Ast
open Ast
open Common.Alarms
module IntItv = Universal.Numeric.Values.Intervals.Integer.Value
module FltItv = Universal.Numeric.Values.Intervals.Float.I



module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.memory.scalars.machine_numbers"
    end)

  let universal = Semantic "Universal"

  let checks = [ CHK_C_INTEGER_OVERFLOW;
                 CHK_C_DIVIDE_BY_ZERO;
                 CHK_C_INVALID_SHIFT;
                 CHK_C_FLOAT_INVALID_OPERATION;
                 CHK_C_FLOAT_DIVISION_BY_ZERO;
                 CHK_C_FLOAT_OVERFLOW
               ]

  (** Command-line options *)
  (** ==================== *)

  let opt_signed_arithmetic_overflow = ref true
  let () =
    register_domain_option name {
      key = "-c-check-signed-arithmetic-overflow";
      category = "C";
      doc = " check overflows in signed integer arithmetic";
      spec = ArgExt.Bool (fun b -> opt_signed_arithmetic_overflow := b);
      default = "true";
    }

  let opt_unsigned_arithmetic_overflow = ref false
  let () =
    register_domain_option name {
      key = "-c-check-unsigned-arithmetic-overflow";
      category = "C";
      doc = " check overflows in unsigned integer arithmetic";
      spec = ArgExt.Bool (fun b -> opt_unsigned_arithmetic_overflow := b);
      default = "false";
    }

  let opt_signed_implicit_cast_overflow = ref true
  let () =
    register_domain_option name {
      key = "-c-check-signed-implicit-cast-overflow";
      category = "C";
      doc = " check overflows in implicit casts to signed integer";
      spec = ArgExt.Bool (fun b -> opt_signed_implicit_cast_overflow := b);
      default = "true";
    }

  let opt_unsigned_implicit_cast_overflow = ref true
  let () =
    register_domain_option name {
      key = "-c-check-unsigned-implicit-cast-overflow";
      category = "C";
      doc = " check overflows in implicit casts to unsigned integer";
      spec = ArgExt.Bool (fun b -> opt_unsigned_implicit_cast_overflow := b);
      default = "true";
    }


  let opt_explicit_cast_overflow = ref false
  let () =
    register_domain_option name {
      key = "-c-check-explicit-cast-overflow";
      category = "C";
      doc = " check overflows in explicit casts";
      spec = ArgExt.Bool (fun b -> opt_explicit_cast_overflow := b);
      default = "false";
    }


  let opt_float_invalid_operation = ref false
  let () =
    register_domain_option name {
      key = "-c-check-float-invalid-operation";
      category = "C";
      doc = " invalid float operations generate errors instead of silent NaN";
      spec = ArgExt.Bool (fun b -> opt_float_invalid_operation := b);
      default = "false";
    }

  let opt_float_division_by_zero = ref false
  let () =
    register_domain_option name {
      key = "-c-check-float-division-by-zero";
      category = "C";
      doc = " float divisions by 0 generate errors instead of infinities";
      spec = ArgExt.Bool (fun b -> opt_float_division_by_zero := b);
      default = "false";
    }

  let opt_float_overflow = ref false
  let () =
    register_domain_option name {
      key = "-c-check-float-overflow";
      category = "C";
      doc = " float overflows generate errors";
      spec = ArgExt.Bool (fun b -> opt_float_overflow := b);
      default = "false";
    }



  (** Numeric variables *)
  (** ================= *)

  (** Kind of mathematical numeric variables encoding the value of C numeric
      variables (integers and floats)
  *)
  type var_kind += V_c_num of var

  let () =
    register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_num vv -> pp_var fmt vv
          | _ -> next fmt v
        );
      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_num vv1, V_c_num vv2 -> compare_var vv1 vv2
          | _ -> next v1 v2
        );
    }

  let to_num_type t =
    match t with
    | T_bool | T_int | T_float _ | T_any -> t
    | _ ->
      match remove_typedef_qual t with
      | T_c_bool -> T_bool
      | T_c_integer _ -> T_int
      | T_c_enum _ -> T_int
      | T_c_float C_float -> T_float F_SINGLE
      | T_c_float C_double -> T_float F_DOUBLE
      | T_c_float C_long_double -> T_float F_LONG_DOUBLE
      | _ -> panic ~loc:__LOC__ "non integer type %a" pp_typ t

  let mk_num_var v =
    let vname = Format.asprintf "num⦅%s⦆" v.vname in
    mkv vname (V_c_num v) (to_num_type v.vtyp) ~mode:v.vmode ~semantic:"Universal"

  let mk_num_var_expr e =
    match ekind e with
    | E_var (v,mode) -> mk_var (mk_num_var v) ~mode e.erange
    | _ -> assert false


  (** Utility functions *)
  (** ================= *)

  let range_leq (a,b) (c,d) =
    Z.leq c a && Z.leq b d

  let wrap_z (z : Z.t) ((l,h) : Z.t * Z.t) : Z.t =
    Z.( l + ((z - l) mod (h-l+one)) )

  let is_c_overflow_op = function
    | O_mult | O_plus | O_minus -> true
    | O_bit_and | O_bit_or | O_bit_xor -> true
    | _ -> false

  let is_c_div_op = function
    | O_div | O_mod -> true
    | _ -> false

  let is_c_shift_op = function
    | O_bit_lshift | O_bit_rshift -> true
    | _ -> false

  let is_c_float_op = function
    | O_plus | O_minus | O_mult | O_div -> true
    | _ -> false


  (** Rebuild a C expression from its parts *)
  let rebuild_c_expr exp parts =
    let _,builder = structure_of_expr exp in
    builder {exprs = parts; stmts = []}


  (** Build a numeric expression from a C expression *)
  let c2num exp =
    let parts,builder = structure_of_expr exp in
    let nparts = { exprs = List.map (get_expr_translation "Universal") parts.exprs; stmts = [] } in
    let e = builder nparts in
    { e with etyp = to_num_type e.etyp }


  (** [check_int_overflow cexp nexp ...] checks whether the C expression
      [cexp] produces an integer overflow and transforms its numeric
      evaluation [nexp] accordingly *)
  let check_int_overflow cexp ?(nexp=c2num cexp) range man flow =
    let typ = cexp.etyp in
    (* Function that performs the actual check *)
    let do_check ?(exp=cexp) raise_alarm =
      let rmin, rmax = rangeof typ in
      let ritv = IntItv.of_z rmin rmax in
      let itv = man.ask (Universal.Numeric.Common.mk_int_interval_query nexp) flow in
      if IntItv.subset itv ritv then
        safe_c_integer_overflow_check range man flow |>
        Eval.singleton cexp |>
        Eval.add_translation "Universal" nexp
      else
        let nexp' = wrap_expr nexp (rmin, rmax) range in
        let flow' =
          if raise_alarm
          then
            if IntItv.meet itv ritv |> IntItv.is_bottom then
              raise_c_integer_overflow_alarm ~warning:false exp nexp typ range man flow flow
            else
              raise_c_integer_overflow_alarm ~warning:true exp nexp typ range man flow flow
          else flow
        in
        Eval.singleton cexp flow' |>
        Eval.add_translation "Universal" nexp'
    in
    match ekind cexp with
    (* Arithmetics on signed integers may overflow *)
    | E_binop _ | E_unop _ when is_c_signed_int_type typ ->
      do_check !opt_signed_arithmetic_overflow

    (* Arithmetics on unsigned integers *)
    | E_binop _ | E_unop _ when not (is_c_signed_int_type typ) ->
      do_check !opt_unsigned_arithmetic_overflow

    (* Implicit casts to signed integers *)
    | E_c_cast(e, false) when is_c_signed_int_type typ ->
      do_check ~exp:e !opt_signed_implicit_cast_overflow

    (* Implicit casts to unsigned integers *)
    | E_c_cast(e, false) when not (is_c_signed_int_type typ) ->
      do_check ~exp:e !opt_unsigned_implicit_cast_overflow

    (* Explicit casts *)
    | E_c_cast(e, true) ->
      do_check ~exp:e !opt_explicit_cast_overflow

    | _ -> panic_at range "check_int_overflow: unsupported expression %a" pp_expr cexp


  (** Check that a division is not performed on a null denominator *)
  let check_int_division cexp man flow =
    let nexp = c2num cexp in
    let denominator = match ekind nexp with
      | E_binop(_,_,e) -> e
      | _ -> assert false in
    let cond = ne denominator zero cexp.erange in
    assume cond
      ~fthen:(fun tflow ->
          safe_c_divide_by_zero_check cexp.erange man tflow |>
          (* a division can also generate an overflow (min-int / -1) *)
          check_int_overflow cexp ~nexp cexp.erange man
        )
      ~felse:(fun fflow ->
          let flow = raise_c_divide_by_zero_alarm denominator cexp.erange man fflow in
          Eval.empty flow
        )
      man flow


  (** Check that bit-shifts are safe. Two conditions are verified:
      (i) the shift position is positive, and
      (ii) this position does not exceed the size of the shifted value
  *)
  let check_int_shift cexp range man flow =
    let t = cexp.etyp in
    let nexp = c2num cexp in
    let n = match ekind nexp with E_binop(_,_,n) -> n | _ -> assert false in
    let range = cexp.erange in
    (* Condition: n ∈ [0, bits(t) - 1] *)
    let bits = sizeof_type t |> Z.mul (Z.of_int 8) in
    let cond = mk_in n (mk_zero range) (mk_z (Z.pred bits) range) range in
    assume cond
      ~fthen:(fun tflow ->
          let tflow = safe_c_shift_check range man tflow in
          check_int_overflow cexp ~nexp range man tflow
        )
      ~felse:(fun fflow ->
          let flow' = raise_c_invalid_shift_alarm cexp n range man flow fflow in
          Eval.empty flow'
        )
      man flow


  (** [check_float_valid cexp nexp ...] checks whether the C expression
      [cexp] produces an infinity or NaN result. *)
  let check_float_valid cexp ?(nexp=c2num cexp) range man flow =
    let typ = cexp.etyp in
    let prec = get_c_float_precision typ in
    let itv = man.ask (Universal.Numeric.Common.mk_float_interval_query ~prec nexp) flow in
    let flow', nexp' =
      if !opt_float_invalid_operation then
        if itv.nan then
          (* invalid operation exception, remove NaN from result *)
          let float_no_nan = float_class ~valid:true ~inf:true () in
          raise_c_float_invalid_operation_alarm cexp itv typ range man flow,
          mk_filter_float_class float_no_nan nexp range
        else
          (* safe, no NaN *)
          safe_c_float_invalid_operation_check range man flow, nexp
      else
        (* no error reporting, keep NaN from result *)
        flow, nexp
    in
    let flow', nexp' =
      if !opt_float_overflow then
        if itv.pinf || itv.minf then
          (* overflow exception, remove infinities from result *)
          let float_no_inf = float_class ~valid:true ~nan:true () in
          raise_c_float_overflow_alarm cexp itv typ range man flow',
          mk_filter_float_class float_no_inf nexp' range
        else
          (* safe, no NaN *)
          safe_c_float_overflow_check range man flow', nexp'
      else
        (* no error reporting, keep NaN from result *)
        flow', nexp'
    in
    Eval.singleton cexp flow' |>
    Eval.add_translation "Universal" nexp'
        

  (** Check that a float division is not performed on a null denominator.
      Also checks for invalid operations and overflows.
   *)
  let check_float_division cexp man flow =
    let typ = cexp.etyp in
    let range = cexp.erange in
    let prec = get_c_float_precision typ in
    let nexp = c2num cexp in
    let denominator = match ekind nexp with
      | E_binop(_,_,e) -> e
      | _ -> assert false in
    let zero = mk_float ~prec 0. range in
    let cond = ne denominator zero range in
    if !opt_float_division_by_zero then
      let itv = man.ask (Universal.Numeric.Common.mk_float_interval_query ~prec denominator) flow in
      if FltItv.contains_zero itv then
        (* division by zero possible, make two cases *)
        assume cond
        ~fthen:(fun tflow ->
          safe_c_float_division_by_zero_check range man tflow |>
          check_float_valid cexp ~nexp range man
        )
        ~felse:(fun fflow ->
          raise_c_float_division_by_zero_alarm denominator itv range man fflow |>
          Eval.empty
        )
        man flow
      else
        (* no division by zero *)
        safe_c_float_division_by_zero_check range man flow |>
        check_float_valid cexp ~nexp range man
    else
      (* division by zero can happen but will result *)
      check_float_valid cexp ~nexp range man flow

    
  (** Transfer functions *)
  (** ================== *)

  let eval exp man flow =
    match ekind exp with
    (* 𝔼⟦ n ⟧ *)
    | E_constant(C_int _ | C_int_interval _ | C_float _ | C_float_interval _) when is_c_num_type exp.etyp ->
      let nexp = { exp with etyp = to_num_type exp.etyp } in
      Eval.singleton exp flow |>
      Eval.add_translation "Universal" nexp |>
      OptionExt.return

    (* 𝔼⟦ 'c' ⟧ *)
    | E_constant(C_c_character (c, _)) ->
      let nexp = {exp with ekind = E_constant (C_int c); etyp = to_num_type exp.etyp} in
      Eval.singleton exp flow |>
      Eval.add_translation "Universal" nexp |>
      OptionExt.return

    (* 𝔼⟦ ⊤int ⟧ *)
    | E_constant(C_top t) when is_c_int_type t ->
      let l, u = rangeof t in
      let nexp = mk_z_interval l u ~typ:(to_num_type t) exp.erange in
      Eval.singleton exp flow |>
      Eval.add_translation "Universal" nexp |>
      OptionExt.return

    (* 𝔼⟦ ⊤float ⟧ *)
    | E_constant(C_top t) when is_c_float_type t ->
      let nexp = mk_top (to_num_type t) exp.erange in
      Eval.singleton exp flow |>
      Eval.add_translation "Universal" nexp |>
      OptionExt.return

    (* 𝔼⟦ var ⟧ *)
    | E_var (v,_) when is_c_num_type v.vtyp ->
      let nexp = mk_num_var_expr exp in
      Eval.singleton exp flow |>
      Eval.add_translation "Universal" nexp |>
      OptionExt.return

    (* 𝔼⟦ ⋄ e ⟧, ⋄ ∈ {+, -} and type(t) = int *)
    | E_unop(op, e) when is_c_overflow_op op &&
                         exp |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e flow ->
      let cexp = rebuild_c_expr exp [e] in
      check_int_overflow cexp exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ ~ e ⟧, type(e) = unsigned *)
    | E_unop(O_bit_invert, e) when exp |> etyp |> is_c_int_type
                                && not (exp |> etyp |> is_c_signed_int_type) ->
      man.eval e flow >>$? fun e flow ->
      let cexp = rebuild_c_expr exp [e] in
      let ne = get_expr_translation "Universal" e in
      let _,hi = rangeof exp.etyp in
      let nexp = sub (mk_z hi exp.erange) ne exp.erange in
      Eval.singleton cexp flow |>
      Eval.add_translation "Universal" nexp |>
      OptionExt.return

    (* 𝔼⟦ ⋄ e ⟧, type(exp) = int *)
    | E_unop(op, e) when exp |> etyp |> is_c_int_type ->
      man.eval e flow >>$? fun e flow ->
      let cexp = rebuild_c_expr exp [e] in
      let nexp = c2num cexp in
      Eval.singleton cexp flow |>
      Eval.add_translation "Universal" nexp |>
      OptionExt.return

    (* 𝔼⟦ ⋄ e' ⟧, type(exp) = float *)
    | E_unop(op, e) when exp |> etyp |> is_c_float_type
      ->
      man.eval e flow >>$? fun e flow ->
      let cexp = rebuild_c_expr exp [e] in
      check_float_valid cexp exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {/, %} and type(exp) = int *)
    | E_binop(op, e, e') when op |> is_c_div_op &&
                              exp |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      let cexp = rebuild_c_expr exp [e;e'] in
      check_int_division cexp man flow |>
      OptionExt.return

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {>>, <<} *)
    | E_binop(op, e, e') when op |> is_c_shift_op &&
                              exp |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      let cexp = rebuild_c_expr exp [e;e'] in
      check_int_shift cexp exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {+, -, *} and type(exp) = int *)
    | E_binop(op, e, e') when is_c_overflow_op op &&
                              exp |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      let cexp = rebuild_c_expr exp [e;e'] in
      check_int_overflow cexp exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ e / e' ⟧, type(exp) = float *)
    | E_binop(O_div, e, e') when exp |> etyp |> is_c_float_type
      ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      let cexp = rebuild_c_expr exp [e;e'] in
      check_float_division cexp man flow |>
      OptionExt.return
                           
    (* 𝔼⟦ e ⋄ e' ⟧, type(exp) = float *)
    | E_binop(op, e, e') when exp |> etyp |> is_c_float_type
      ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      let cexp = rebuild_c_expr exp [e;e'] in
      check_float_valid cexp exp.erange man flow |>
      OptionExt.return
                           
    (* 𝔼⟦ e ⋄ e' ⟧ *)
    | E_binop(op, e, e') when exp |> etyp |> is_c_num_type ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      let cexp = rebuild_c_expr exp [e;e'] in
      let nexp = c2num cexp in
      (* Re-evaluate the numeric expression, in case Universal wants to
         transform it (e.g. comparison expressions can be transformed into
         booleans *)
      man.eval nexp flow >>$? fun nexp flow ->
      (* Return a C constant if Universal computed a constant *)
      let cexp' =
        match expr_to_z nexp with
        | None   -> cexp
        | Some z -> mk_z z ~typ:cexp.etyp cexp.erange in
      Eval.singleton cexp' flow |>
      Eval.add_translation "Universal" nexp |>
      OptionExt.return

    (* 𝔼⟦ (bool)e ⟧ *)
    | E_c_cast(e,_) when is_c_bool_type exp.etyp &&
                         is_c_int_type e.etyp ->
      assume e man flow
        ~fthen:(Eval.singleton
                  (mk_one ~typ:exp.etyp exp.erange)
                  ~translations:["Universal",mk_true exp.erange])
        ~felse:(Eval.singleton
                  (mk_zero ~typ:exp.etyp exp.erange)
                  ~translations:["Universal",mk_false exp.erange])
      |>
      OptionExt.return

    (* 𝔼⟦ (int)int ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type &&
                          e   |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e flow ->
      let cexp = rebuild_c_expr exp [e] in
      check_int_overflow cexp ~nexp:(get_expr_translation "Universal" e) exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ (int)float ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type &&
                          e   |> etyp |> is_c_float_type
      ->
      man.eval e flow >>$? fun e flow ->
      let cexp = rebuild_c_expr exp [e] in
      let nexp =
        mk_unop O_cast
          (get_expr_translation "Universal" e)
          ~etyp:(to_num_type exp.etyp) exp.erange
      in
      check_int_overflow cexp ~nexp exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ (float)num ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_float_type &&
                          e   |> etyp |> is_c_num_type ->
      man.eval e flow >>$? fun e flow ->
      let cexp = rebuild_c_expr exp [e] in
      let nexp =
        mk_unop O_cast
          (get_expr_translation "Universal" e)
          ~etyp:(to_num_type exp.etyp) exp.erange
      in
      check_float_valid cexp ~nexp exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ (int)num ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type &&
                          is_numeric_type e.etyp ->
      man.eval e flow >>$? fun e flow ->
      Eval.singleton exp flow |>
      Eval.add_translation "Universal" e |>
      OptionExt.return

    (* 𝔼⟦ __builtin_isfinite(e) ⟧ *)
    | E_c_builtin_call("__builtin_isfinite", [e]) ->
       man.eval e flow >>$? fun e flow ->
       assume (mk_float_class float_valid e exp.erange)
         ~fthen:(Eval.singleton (mk_one exp.erange)) (* should be !=0 *)
         ~felse:(Eval.singleton (mk_zero exp.erange))
         man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isnan(e) ⟧ *)
    | E_c_builtin_call("__builtin_isnan", [e]) ->
       man.eval e flow >>$? fun e flow ->
       assume (mk_float_class float_nan e exp.erange)
         ~fthen:(Eval.singleton (mk_one exp.erange)) (* should be !=0 *)
         ~felse:(Eval.singleton (mk_zero exp.erange))
         man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isnormal(e) ⟧ *)
    | E_c_builtin_call("__builtin_isnormal", [e]) ->
       (* !valid => !normal but valid =/=> normal *)
       man.eval e flow >>$? fun e flow ->
       assume (mk_float_class float_valid e exp.erange)
         ~fthen:(Eval.singleton (mk_top s32 exp.erange))
         ~felse:(Eval.singleton (mk_zero exp.erange))
         man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isinf_sign(e) ⟧ *)
    | E_c_builtin_call("__builtin_isinf_sign", [e]) ->
       man.eval e flow >>$? fun e flow ->
       assume (mk_float_class float_inf e exp.erange)
         ~fthen:(
           switch [
               [mk_gt e (mk_float 0. exp.erange) exp.erange],
               Eval.singleton (mk_one exp.erange);
               [mk_lt e (mk_float 0. exp.erange) exp.erange],
               Eval.singleton (mk_minus_one exp.erange);
               [mk_eq e (mk_float 0. exp.erange) exp.erange],
               Eval.singleton (mk_zero exp.erange)
             ] man
         )
         ~felse:(Eval.singleton (mk_zero exp.erange))
         man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_huge_val() ⟧,
       𝔼⟦ __builtin_huge_inf() ⟧ *)
    | E_c_builtin_call("__builtin_huge_val", [])
    | E_c_builtin_call("__builtin_inff", []) ->
       Eval.singleton (mk_float infinity exp.erange) flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_huge_valf() ⟧ *)
    | E_c_builtin_call("__builtin_huge_valf", []) ->
       Eval.singleton (mk_float ~prec:F_SINGLE infinity exp.erange) flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_huge_vall() ⟧ *)
    | E_c_builtin_call("__builtin_huge_vall", []) ->
       Eval.singleton (mk_float ~prec:F_LONG_DOUBLE infinity exp.erange) flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_nanf("") ⟧ *)
    | E_c_builtin_call("__builtin_nanf", [_]) ->
       Eval.singleton (mk_float ~prec:F_SINGLE nan exp.erange) flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_signbit(e) ⟧ *)
    | E_c_builtin_call("__builtin_signbit", [e]) ->
       man.eval e flow >>$? fun e flow ->
       switch [
           [mk_ge e (mk_float 0. exp.erange) exp.erange],
           Eval.singleton (mk_zero exp.erange);
           [mk_le e (mk_float 0. exp.erange) exp.erange],
           Eval.singleton (mk_top s32 exp.erange); (* should be !=0 *)
           (* 0. should indeed return top because it can represent +0. or -0. *)
         ] man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_fpclassify(...) ⟧ *)
    | E_c_builtin_call("__builtin_fpclassify", [fp_nan;fp_infinite;fp_normal;fp_subnormal;fp_zero;e]) ->
       man.eval fp_nan flow >>$? fun fp_nan flow ->
       man.eval fp_infinite flow >>$? fun fp_infinite flow ->
       man.eval fp_normal flow >>$? fun fp_normal flow ->
       man.eval fp_subnormal flow >>$? fun fp_subnormal flow ->
       man.eval fp_zero flow >>$? fun fp_zero flow ->
       man.eval e flow >>$? fun e flow ->
       switch [
           (* FP_NAN *)
           [mk_float_class float_nan e exp.erange],
           Eval.singleton fp_nan;
           (* FP_INFINITE *)
           [mk_float_class float_inf e exp.erange],
           Eval.singleton fp_infinite;
           (* FP_ZERO *)
           [mk_eq e (mk_float 0. exp.erange) exp.erange],
           Eval.singleton fp_zero;
           (* FP_SUBNORMAL *)
           [mk_float_class float_valid e exp.erange], (* should also be != 0 *)
           Eval.singleton fp_subnormal;
           (* FP_NORMAL *)
           [mk_float_class float_valid e exp.erange], (* should also be != 0 *)
           Eval.singleton fp_normal
         ] man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isgreater(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_isgreater", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_gt e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isgreaterequal(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_isgreaterequal", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_ge e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isless(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_isless", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_lt e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_islessequal(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_islessequal", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_gt e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_islessgreater(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_islessgreater", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_zero exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_zero exp.erange))
             ~felse:(Eval.singleton (mk_ne e1 e2 exp.erange))
             man
         ) man flow |>
         OptionExt.return

    (* 𝔼⟦ __builtin_isunordered(e1, e2) ⟧ *)
    | E_c_builtin_call("__builtin_isunordered", [e1;e2]) ->
       man.eval e1 flow >>$? fun e1 flow ->
       man.eval e2 flow >>$? fun e2 flow ->
       assume (mk_float_class float_nan e1 exp.erange)
         ~fthen:(Eval.singleton (mk_one exp.erange))
         ~felse:(
           assume (mk_float_class float_nan e2 exp.erange)
             ~fthen:(Eval.singleton (mk_one exp.erange))
             ~felse:(Eval.singleton (mk_zero exp.erange))
             man
         ) man flow |>
         OptionExt.return

    | _ ->
      None


  let add_var_bounds vv t flow =
    if is_c_int_type t then
      let l,u = rangeof t in
      let vv = match ekind vv with E_var (vv, _) -> vv | _ -> assert false in
      Framework.Combiners.Value.Nonrel.add_var_bounds_flow vv (C_int_interval (ItvUtils.IntBound.Finite l,ItvUtils.IntBound.Finite u)) flow
    else
      flow


  (* Declaration of a scalar numeric variable *)
  let declare_var v init scope range man flow =
    let vv = mk_var (mk_num_var v) range in

    let init' =
      match scope, init with
      (** Uninitialized global variable *)
      | Variable_global, None | Variable_file_static _, None ->
        (* The variable is initialized with 0 (C99 6.7.8.10) *)
        Eval.singleton (mk_zero range) flow

      (** Uninitialized local variable *)
      | Variable_local _, None | Variable_func_static _, None ->
        (* The value of the variable is undetermined (C99 6.7.8.10) *)
        if is_c_int_type v.vtyp then
          let l,u = rangeof v.vtyp in
          Eval.singleton (mk_z_interval l u range) flow
        else
          Eval.singleton (mk_top (to_num_type v.vtyp) range) flow

      | _, Some (C_init_expr e) ->
        man.eval ~translate:"Universal" e flow

      | _ -> assert false
    in

    init' >>$ fun init' flow ->
    man.exec (mk_add vv range) flow ~route:universal >>% fun flow ->
    add_var_bounds vv v.vtyp flow |>
    man.exec (mk_assign vv init' range) ~route:universal


  let exec stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) when is_c_num_type v.vtyp ->
      declare_var v init scope stmt.srange man flow |>
      OptionExt.return

    | S_assign({ekind = E_var _} as lval, rval) when etyp lval |> is_c_num_type ->
      man.eval ~translate:"Universal" lval flow >>$? fun lval' flow ->
      man.eval ~translate:"Universal" rval flow >>$? fun rval' flow ->
      man.exec (mk_assign lval' rval' stmt.srange) flow ~route:universal |>
      OptionExt.return

    | S_assume(e) when is_c_num_type e.etyp ->
      man.eval ~translate:"Universal" e flow >>$? fun e' flow ->
      begin match expr_to_z e' with
        | Some n when Z.(n = zero) -> Post.return (Flow.remove T_cur flow)
        | Some n                   -> Post.return flow
        | None                     -> man.exec (mk_assume e' stmt.srange) flow ~route:universal
      end |>
      OptionExt.return

    | S_add ({ekind = E_var _} as v) when is_c_num_type v.etyp ->
      let vv = mk_num_var_expr v in
      add_var_bounds vv v.etyp flow |>
      man.exec (mk_add vv stmt.srange) ~route:universal |>
      OptionExt.return

    | S_remove ({ekind = E_var _} as v) when is_c_num_type v.etyp ->
      let vv = mk_num_var_expr v in
      man.exec (mk_remove vv stmt.srange) flow ~route:universal |>
      OptionExt.return

    | S_rename(({ekind = E_var _} as v1), ({ekind = E_var _} as v2))
      when is_c_num_type v1.etyp &&
           is_c_num_type v2.etyp
      ->
      let vv1 = mk_num_var_expr v1 in
      let vv2 = mk_num_var_expr v2 in
      man.exec (mk_rename vv1 vv2 stmt.srange) flow ~route:universal |>
      OptionExt.return

    | S_expand({ekind = E_var _} as e, el)
      when is_c_num_type e.etyp &&
           List.for_all (fun ee -> is_c_num_type ee.etyp) el
      ->
      let v = mk_num_var_expr e in
      let vl = List.map mk_num_var_expr el in
      man.exec (mk_expand v vl stmt.srange) flow ~route:universal |>
      OptionExt.return

    | S_fold(({ekind = E_var _} as e),el)
      when is_c_num_type e.etyp &&
           List.for_all (fun ee -> is_c_num_type ee.etyp) el
      ->
      let v = mk_num_var_expr e in
      let vl = List.map mk_num_var_expr el in
      man.exec (mk_fold v vl stmt.srange) flow ~route:universal |>
      OptionExt.return

    | S_forget ({ekind = E_var _} as v) when is_c_num_type v.etyp ->
      let vv = mk_num_var_expr v in
      let top =
        if is_c_int_type v.etyp then
          let lo,hi = rangeof v.etyp in
          mk_z_interval lo hi stmt.srange
        else
          mk_top vv.etyp stmt.srange
      in
      man.exec (mk_assign vv top stmt.srange) flow ~route:universal |>
      OptionExt.return

    | _ -> None


  let ask _ _ _ =
    None

  let init _ _ flow =  flow


  (** {2 Pretty printer} *)
  (** ****************** *)

  let print_expr man flow printer exp =
    match ekind (remove_casts exp) with
    | E_var (v,_) when is_c_num_type v.vtyp ->
      let vv = mk_num_var v in
      man.print_expr flow printer (mk_var vv exp.erange)
    | _ -> ()

end

let () =
  register_stateless_domain (module Domain)
