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
module Itv = Universal.Numeric.Values.Intervals.Integer.Value



module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.memory.scalars.machine_numbers"
    end)

  let numeric = Semantic "U/Numeric"

  let checks = [ CHK_C_INTEGER_OVERFLOW;
                 CHK_C_DIVIDE_BY_ZERO;
                 CHK_C_INVALID_SHIFT ]

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
    mkv v.vname (V_c_num v) (to_num_type v.vtyp) ~mode:v.vmode ~semantic:"U/Numeric"

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

  (** [check_overflow cexp nexp ...] checks whether the C expression
      [cexp] produces an integer overflow and returns and transforms
      its numeric evaluation [nexp] accordingly *)
  let check_overflow cexp nexp range man flow =
    let typ = cexp.etyp in
    (* Function that performs the actual check *)
    let do_check raise_alarm =
      let rmin, rmax = rangeof typ in
      let ritv = Itv.of_z rmin rmax in
      let itv = man.ask (Universal.Numeric.Common.mk_int_interval_query nexp) flow in
      if Itv.is_bottom itv then
        unreachable_c_integer_overflow_check range man flow |>
        Eval.singleton nexp
      else
      if Itv.subset itv ritv then
        safe_c_integer_overflow_check range man flow |>
        Eval.singleton nexp
      else
        let nexp' = wrap_expr nexp (rmin, rmax) range in
        let flow' =
          if raise_alarm
          then
            if Itv.meet itv ritv |> Itv.is_bottom then
              raise_c_integer_overflow_alarm ~warning:false cexp nexp man flow flow
            else
              raise_c_integer_overflow_alarm ~warning:true cexp nexp man flow flow
          else flow
        in
        Eval.singleton nexp' flow'
    in
    match ekind cexp with
    (* Arithmetics on signed integers may overflow *)
    | E_binop _ | E_unop _ when is_c_signed_int_type typ ->
      do_check !opt_signed_arithmetic_overflow

    (* Arithmetics on unsigned integers *)
    | E_binop _ | E_unop _ when not (is_c_signed_int_type typ) ->
      do_check !opt_unsigned_arithmetic_overflow

    (* Implicit casts to signed integers *)
    | E_c_cast(_, false) when is_c_signed_int_type typ ->
      do_check !opt_signed_implicit_cast_overflow

    (* Implicit casts to unsigned integers *)
    | E_c_cast(_, false) when not (is_c_signed_int_type typ) ->
      do_check !opt_unsigned_implicit_cast_overflow

    (* Explicit casts *)
    | E_c_cast(_, true) ->
      do_check !opt_explicit_cast_overflow

    | _ -> panic_at range "check_overflow: unsupported expression %a" pp_expr cexp


  (** Check that a division is not performed on a null denominator *)
  let check_division op nominator denominator range man flow =
    let cond = ne denominator zero range in
    assume cond
      ~fthen:(fun tflow ->
          let tflow = safe_c_divide_by_zero_check range man tflow in
          let exp' = mk_binop nominator op denominator ~etyp:T_int range in
          Eval.singleton exp' tflow
        )
      ~felse:(fun fflow ->
          let flow = raise_c_divide_by_zero_alarm denominator range man fflow in
          Eval.empty flow
        )
      ~fnone:(fun nflow ->
          let nflow = unreachable_c_divide_by_zero_check range man nflow in
          Eval.empty nflow
        )
      ~route:numeric man flow


  (** Check that bit-shifts are safe. Two conditions are verified:
      (i) the shift position is positive, and
      (ii) this position does not exceed the size of the shifted value
  *)
  let check_shift exp e n range man flow =
    let t = exp.etyp in
    let op = match ekind exp with E_binop(op,_,_) -> op | _ -> assert false in
    (* Condition: n ∈ [0, bits(t) - 1] *)
    let bits = sizeof_type t |> Z.mul (Z.of_int 8) in
    let cond = mk_in n (mk_zero range) (mk_z (Z.pred bits) range) range in
    assume cond
      ~fthen:(fun tflow ->
          let tflow = safe_c_shift_check range man tflow in
          let exp' = { exp with
                       ekind = E_binop(op,e,n);
                       etyp  = to_num_type t; }
          in
          check_overflow exp exp' range man tflow
        )
      ~felse:(fun fflow ->
          let flow' = raise_c_invalid_shift_alarm exp n man flow fflow in
          Eval.empty flow'
        )
      ~fnone:(fun nflow ->
          let nflow = unreachable_c_shift_check range man nflow in
          Eval.empty nflow
        )
      ~route:numeric man flow


  (** Transfer functions *)
  (** ================== *)


  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    (* 𝔼⟦ n ⟧ *)
    | E_constant(C_int _ | C_int_interval _ | C_float _ | C_float_interval _) when is_c_num_type exp.etyp ->
      Eval.singleton {exp with etyp = to_num_type exp.etyp} flow
      |> OptionExt.return

    (* 𝔼⟦ 'c' ⟧ *)
    | E_constant(C_c_character (c, _)) ->
      Eval.singleton {exp with ekind = E_constant (C_int c); etyp = to_num_type exp.etyp} flow
      |> OptionExt.return

    (* 𝔼⟦ ⊤int ⟧ *)
    | E_constant(C_top t) when is_c_int_type t ->
      let l, u = rangeof t in
      let exp' = mk_z_interval l u ~typ:(to_num_type t) exp.erange in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ ⊤float ⟧ *)
    | E_constant(C_top t) when is_c_float_type t ->
      let exp' = mk_top (to_num_type t) exp.erange in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ var ⟧ *)
    | E_var (v,_) when is_c_num_type v.vtyp ->
      Eval.singleton (mk_num_var_expr exp) flow |>
      OptionExt.return

    (* 𝔼⟦ ⋄ e ⟧, ⋄ ∈ {+, -} and type(t) = int *)
    | E_unop(op, e) when is_c_overflow_op op &&
                         exp |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e flow ->
      let exp' = { exp with
                   ekind = E_unop(op, e);
                   etyp = to_num_type exp.etyp } in
      check_overflow exp exp' range man flow |>
      OptionExt.return

    (* 𝔼⟦ ⋄ e ⟧, ⋄ ∈ {+, -} and type(t) = int *)
    | E_unop(O_wrap(min,max) as op, e) when exp |> etyp |> is_c_int_type ->
      man.eval e flow >>$? fun e flow ->
      let exp' = { exp with
                   ekind = E_unop(op, e);
                   etyp = to_num_type exp.etyp } in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ ~ e ⟧, type(e) = unsigned *)
    | E_unop(O_bit_invert, e) when exp |> etyp |> is_c_int_type
                                && not (exp |> etyp |> is_c_signed_int_type) ->
      man.eval e flow >>$? fun e flow ->
      let _,hi = rangeof exp.etyp in
      Eval.singleton (sub (mk_z hi exp.erange) e exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ ⋄ e ⟧ *)
    | E_unop(op, e) when exp |> etyp |> is_c_num_type ->
      man.eval e flow >>$? fun e flow ->
      let exp' = { exp with
                   ekind = E_unop(op, e);
                   etyp = to_num_type exp.etyp } in
      Eval.singleton exp' flow |>
      OptionExt.return


    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {/, %} and type(exp) = int *)
    | E_binop(op, e, e') when op |> is_c_div_op &&
                              exp |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      check_division op e e' range man flow |>
      OptionExt.return

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {>>, <<} *)
    | E_binop(op, e, e') when op |> is_c_shift_op &&
                              exp |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      check_shift exp e e' range man flow |>
      OptionExt.return

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {+, -, *} and type(exp) = int *)
    | E_binop(op, e, e') when is_c_overflow_op op &&
                              exp |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      let exp' = { exp with
                   ekind = E_binop(op, e, e');
                   etyp = to_num_type exp.etyp }
      in
      check_overflow exp exp' range man flow |>
      OptionExt.return

    (* 𝔼⟦ e ⋄ e' ⟧ *)
    | E_binop(op, e, e') when exp |> etyp |> is_c_num_type ->
      man.eval e flow >>$? fun e flow ->
      man.eval e' flow >>$? fun e' flow ->
      let exp' = { exp with
                   ekind = E_binop(op, e, e');
                   etyp = to_num_type exp.etyp }
      in
      (* Re-evaluate the numeric expression, in case Universal wants to
         transform it (e.g. comparison expressions can be transformed into
         booleans *)
      man.eval exp' flow |>
      OptionExt.return

    | E_c_cast(e,_) when is_c_bool_type exp.etyp &&
                         is_c_int_type e.etyp ->
      assume e man flow
        ~fthen:(Eval.singleton (mk_true exp.erange))
        ~felse:(Eval.singleton (mk_false exp.erange))
      |>
      OptionExt.return


    (* 𝔼⟦ (float)int ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_float_type &&
                          e   |> etyp |> is_c_int_type ->
      man.eval e flow >>$? fun e flow ->
      let exp' =
        mk_unop
          O_cast
          e ~etyp:(to_num_type exp.etyp) exp.erange
      in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ (int)float ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type &&
                          e   |> etyp |> is_c_float_type ->
      man.eval e flow >>$? fun e flow ->
      let exp' =
        mk_unop
          O_cast
          e ~etyp:(to_num_type exp.etyp) exp.erange
      in
      Eval.singleton exp' flow |>
      OptionExt.return


    (* 𝔼⟦ (int)int ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type &&
                          e   |> etyp |> is_c_int_type
      ->
      man.eval e flow >>$? fun e' flow ->
      check_overflow exp e' range man flow |>
      OptionExt.return

    (* 𝔼⟦ (int)num ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type &&
                          is_numeric_type e.etyp ->
      Eval.singleton e flow |>
      OptionExt.return

    (* 𝔼⟦ (float)float ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_float_type &&
                          e   |> etyp |> is_c_float_type ->
      man.eval e flow |>
      OptionExt.return

    (* 𝔼⟦ valid_float(e) ⟧ *)
    | Stubs.Ast.E_stub_builtin_call(VALID_FLOAT as f, e) ->
      man.eval e flow >>$? fun e flow ->
      Eval.singleton (mk_expr (E_stub_builtin_call(f, e)) ~etyp:exp.etyp exp.erange) flow |>
      OptionExt.return

    | _ ->
      None


  let add_var_bounds vv t flow =
    if is_c_int_type t then
      let l,u = rangeof t in
      let vv = match ekind vv with E_var (vv, _) -> vv | _ -> assert false in
      Framework.Combiners.Value.Nonrel.add_var_bounds_flow vv (C_int_interval (l,u)) flow
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
        man.eval e flow

      | _ -> assert false
    in

    init' >>$ fun init' flow ->
    man.exec ~route:numeric (mk_add vv range) flow >>% fun flow ->
    add_var_bounds vv v.vtyp flow |>
    man.exec ~route:numeric (mk_assign vv init' range)


  let exec stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) when is_c_num_type v.vtyp ->
      declare_var v init scope stmt.srange man flow |>
      OptionExt.return

    | S_assign({ekind = E_var _} as lval, rval) when etyp lval |> is_c_num_type ->
      man.eval lval flow >>$? fun lval' flow ->
      man.eval rval flow >>$? fun rval' flow ->
      man.exec ~route:numeric (mk_assign lval' rval' stmt.srange) flow |>
      OptionExt.return

    | S_add ({ekind = E_var _} as v) when is_c_num_type v.etyp ->
      let vv = mk_num_var_expr v in
      add_var_bounds vv v.etyp flow |>
      man.exec ~route:numeric (mk_add vv stmt.srange) |>
      OptionExt.return

    | S_remove ({ekind = E_var _} as v) when is_c_num_type v.etyp ->
      let vv = mk_num_var_expr v in
      man.exec ~route:numeric (mk_remove vv stmt.srange) flow |>
      OptionExt.return

    | S_rename(({ekind = E_var _} as v1), ({ekind = E_var _} as v2))
      when is_c_num_type v1.etyp &&
           is_c_num_type v2.etyp
      ->
      let vv1 = mk_num_var_expr v1 in
      let vv2 = mk_num_var_expr v2 in
      man.exec ~route:numeric (mk_rename vv1 vv2 stmt.srange) flow |>
      OptionExt.return

    | S_expand({ekind = E_var _} as e, el)
      when is_c_num_type e.etyp &&
           List.for_all (fun ee -> is_c_num_type ee.etyp) el
      ->
      let v = mk_num_var_expr e in
      let vl = List.map mk_num_var_expr el in
      man.exec (mk_expand v vl stmt.srange) ~route:numeric flow |>
      OptionExt.return

    | S_fold(({ekind = E_var _} as e),el)
      when is_c_num_type e.etyp &&
           List.for_all (fun ee -> is_c_num_type ee.etyp) el
      ->
      let v = mk_num_var_expr e in
      let vl = List.map mk_num_var_expr el in
      man.exec (mk_fold v vl stmt.srange) ~route:numeric flow |>
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
      man.exec (mk_assign vv top stmt.srange) ~route:numeric flow |>
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
      man.print_expr ~route:numeric flow printer (mk_var vv exp.erange)
    | _ -> ()

end

let () =
  register_stateless_domain (module Domain)
