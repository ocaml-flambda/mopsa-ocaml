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
open Framework.Core.Sig.Stacked.Stateless
open Universal.Ast
open Stubs.Ast
open Ast
open Zone
open Universal.Zone
open Common.Alarms
open Common.Points_to
module Itv = Universal.Numeric.Values.Intervals.Integer.Value



module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.memory.scalars.machine_numbers"
    end)

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [Z_c_scalar];
      uses = [
        Z_c_scalar;
        Z_u_num
      ];
    };
    ieval = {
      provides = [Z_c_scalar, Z_u_num];
      uses = [
        Z_c_scalar, Z_u_num;
        Z_c_scalar, Z_c_points_to;
        Z_c, Z_u_num
      ];
    }
  }

  let alarms = [ A_c_integer_overflow;
                 A_c_divide_by_zero;
                 A_c_invalid_shift ]

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
    mkv v.vname (V_c_num v) (to_num_type v.vtyp) ~mode:v.vmode

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
      if Itv.is_bottom itv then Eval.empty_singleton flow else
      if Itv.subset itv ritv then Eval.singleton nexp flow else
        let nexp' = wrap_expr nexp (rmin, rmax) range in
        let flow' =
          if raise_alarm
          then raise_c_integer_overflow_alarm cexp nexp man flow flow
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
          let exp' = mk_binop nominator op denominator ~etyp:T_int range in
          Eval.singleton exp' tflow
        )
      ~felse:(fun fflow ->
          let flow = raise_c_divide_by_zero_alarm denominator range man fflow in
          Eval.empty_singleton flow
        )
      ~zone:Z_u_num man flow


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
          let exp' = { exp with
                       ekind = E_binop(op,e,n);
                       etyp  = to_num_type t; }
          in
          Eval.singleton exp' tflow
        )
      ~felse:(fun fflow ->
          let flow' = raise_c_invalid_shift_alarm exp n man flow fflow in
          Eval.empty_singleton flow'
        )
      ~zone:Z_u_num man flow


  let rec is_compare_expr e =
    match ekind e with
    | E_binop(op, e1, e2) when is_comparison_op op -> true
    | E_binop(op, e1, e2) when is_logic_op op -> true
    | E_unop(O_log_not, ee) -> is_compare_expr ee
    | E_c_cast(ee,_) -> is_compare_expr ee
    | _ -> false


  let is_predicate_op = function
    | O_float_class _ -> true
    | _ -> false

  let rec to_compare_expr e =
    match ekind e with
    | E_binop(op, e1, e2) when is_comparison_op op ->
      e

    | E_binop(op, e1, e2) when is_logic_op op ->
      { e with ekind = E_binop(op, to_compare_expr e1, to_compare_expr e2) }

    | E_unop(O_log_not, ee) ->
      { e with ekind = E_unop(O_log_not, to_compare_expr ee) }

    | E_unop (op, _) when is_predicate_op op ->
      e

    | _ ->
      mk_binop e O_ne (mk_zero e.erange) e.erange


  (** Transfer functions *)
  (** ================== *)


  let eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
    (* 𝔼⟦ n ⟧ *)
    | E_constant(C_int _ | C_int_interval _ | C_float _ | C_float_interval _) ->
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
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e flow ->
      let exp' = { exp with
                   ekind = E_unop(op, e);
                   etyp = to_num_type exp.etyp } in
      check_overflow exp exp' range man flow |>
      OptionExt.return

    (* 𝔼⟦ ⋄ e ⟧, ⋄ ∈ {+, -} and type(t) = int *)
    | E_unop(O_wrap(min,max) as op, e) when exp |> etyp |> is_c_int_type ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e flow ->
      let exp' = { exp with
                   ekind = E_unop(op, e);
                   etyp = to_num_type exp.etyp } in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ ! e ⟧ *)
    | E_unop(O_log_not, e) when exp |> etyp |> is_c_num_type ->
      man.eval ~zone:(Z_c_scalar,Z_u_num) e flow >>$? fun e flow ->
      let exp' =
        if is_compare_expr e then
          { exp with
            ekind = E_unop(O_log_not, e);
            etyp = T_bool }
        else
          mk_binop e O_eq (mk_zero exp.erange) exp.erange ~etyp:T_bool
      in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ ⋄ e ⟧ *)
    | E_unop(op, e) when exp |> etyp |> is_c_num_type ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e flow ->
      let exp' = { exp with
                   ekind = E_unop(op, e);
                   etyp = to_num_type exp.etyp } in
      Eval.singleton exp' flow |>
      OptionExt.return


    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {/, %} and type(exp) = int *)
    | E_binop(op, e, e') when op |> is_c_div_op &&
                              exp |> etyp |> is_c_int_type
      ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e flow ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e' flow >>$? fun e' flow ->
      check_division op e e' range man flow |>
      OptionExt.return

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {>>, <<} *)
    | E_binop(op, e, e') when op |> is_c_shift_op &&
                              exp |> etyp |> is_c_int_type
      ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e flow ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e' flow >>$? fun e' flow ->
      check_shift exp e e' range man flow |>
      OptionExt.return

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {+, -, *} and type(exp) = int *)
    | E_binop(op, e, e') when is_c_overflow_op op &&
                              exp |> etyp |> is_c_int_type
      ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e flow ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e' flow >>$? fun e' flow ->
      let exp' = { exp with
                   ekind = E_binop(op, e, e');
                   etyp = to_num_type exp.etyp }
      in
      check_overflow exp exp' range man flow |>
      OptionExt.return

    (* 𝔼⟦ e ⋄ e' ⟧ *)
    | E_binop(op, e, e') when exp |> etyp |> is_c_num_type ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e flow ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e' flow >>$? fun e' flow ->
      let exp' = { exp with
                   ekind = E_binop(op, e, e');
                   etyp = to_num_type exp.etyp }
      in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ (float)int ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_float_type &&
                          e   |> etyp |> is_c_int_type ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e flow ->
      let exp' =
        mk_unop
          (O_cast (to_num_type e.etyp, to_num_type exp.etyp))
          e ~etyp:(to_num_type exp.etyp) exp.erange
      in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ (int)float ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type &&
                          e   |> etyp |> is_c_float_type ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e flow ->
      let exp' =
        mk_unop
          (O_cast (to_num_type e.etyp, to_num_type exp.etyp))
          e ~etyp:(to_num_type exp.etyp) exp.erange
      in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ (int)ptr ⟧ *)
    | E_c_cast(p, _) when exp |> etyp |> is_c_int_type &&
                          p   |> etyp |> is_c_pointer_type ->
      man.eval ~zone:(Z_c_scalar, Z_c_points_to) p flow >>$? fun pt flow ->
      let exp' =
        match ekind pt with
        | E_c_points_to P_null -> mk_zero exp.erange
        | _ ->
          let l,u = rangeof exp.etyp in
          mk_z_interval l u exp.erange
      in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ (int)int ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type &&
                          e   |> etyp |> is_c_int_type
      ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e' flow ->
      check_overflow exp e' range man flow |>
      OptionExt.return

    (* 𝔼⟦ (int)num ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type &&
                          is_numeric_type e.etyp ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow |>
      OptionExt.return

    (* 𝔼⟦ (float)float ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_float_type &&
                          e   |> etyp |> is_c_float_type ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow |>
      OptionExt.return

    (* 𝔼⟦ valid_float(e) ⟧ *)
    | Stubs.Ast.E_stub_builtin_call(VALID_FLOAT as f, e) ->
      man.eval ~zone:(Z_c_scalar,Z_u_num) e flow >>$? fun e flow ->
      Eval.singleton (mk_expr (E_stub_builtin_call(f, e)) ~etyp:exp.etyp exp.erange) flow |>
      OptionExt.return

    (* 𝔼⟦ ∃v ⟧ *)
    | Stubs.Ast.E_stub_quantified(_,v,S_interval _) ->
      man.eval (mk_var v exp.erange) ~zone:(Z_c,Z_u_num) flow |>
      OptionExt.return

    | _ ->
      None


  let add_var_bounds vv t flow =
    if is_c_int_type t then
      let l,u = rangeof t in
      let vv = match ekind vv with E_var (vv, _) -> vv | _ -> assert false in
      Framework.Transformers.Value.Nonrel.add_var_bounds_flow vv (C_int_interval (l,u)) flow
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
        if not (is_compare_expr e) then
          man.eval ~zone:(Z_c_scalar,Z_u_num) e flow
        else
          assume e ~zone:Z_c_scalar
            ~fthen:(fun flow ->
                Eval.singleton (mk_one range) flow
              )
            ~felse:(fun flow ->
                Eval.singleton (mk_zero range) flow
              )
            man flow

      | _ -> assert false
    in

    init' >>$ fun init' flow ->
    man.post ~zone:Z_u_num (mk_add vv range) flow >>= fun _ flow ->
    add_var_bounds vv v.vtyp flow |>
    man.post ~zone:Z_u_num (mk_assign vv init' range)


  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) when is_c_num_type v.vtyp ->
      declare_var v init scope stmt.srange man flow |>
      OptionExt.return

    | S_assign(lval, rval) when etyp lval |> is_c_num_type &&
                                is_compare_expr rval ->
      let range = stmt.srange in
      assume rval ~zone:Z_c_scalar
        ~fthen:(fun flow ->
            man.post ~zone:Z_c_scalar (mk_assign lval (mk_one ~typ:rval.etyp range) range) flow
          )
        ~felse:(fun flow ->
            man.post ~zone:Z_c_scalar (mk_assign lval (mk_zero ~typ:rval.etyp range) range) flow
          )
        man flow |>
      OptionExt.return

    | S_assign(lval, rval) when etyp lval |> is_c_num_type ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) lval flow >>$? fun lval' flow ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) rval flow >>$? fun rval' flow ->
      man.post ~zone:Z_u_num (mk_assign lval' rval' stmt.srange) flow |>
      OptionExt.return

    | S_add v when is_c_num_type v.etyp ->
      let vv = mk_num_var_expr v in
      add_var_bounds vv v.etyp flow |>
      man.post ~zone:Z_u_num (mk_add vv stmt.srange) |>
      OptionExt.return

    | S_remove v when is_c_num_type v.etyp ->
      let vv = mk_num_var_expr v in
      man.post ~zone:Z_u_num (mk_remove vv stmt.srange) flow |>
      Post.bind (fun flow ->
          if is_c_int_type v.etyp then
            let vv = match ekind vv with E_var (vv,_) -> vv | _ -> assert false in
            Framework.Transformers.Value.Nonrel.remove_var_bounds_flow vv flow |>
            Post.return
          else
            Post.return flow
        ) |>
      OptionExt.return

    | S_rename(v1, v2) when is_c_num_type v1.etyp &&
                            is_c_num_type v2.etyp
      ->
      let vv1 = mk_num_var_expr v1 in
      let vv2 = mk_num_var_expr v2 in
      man.post ~zone:Z_u_num (mk_rename vv1 vv2 stmt.srange) flow |>
      OptionExt.return


    | S_assume(e) when is_c_num_type e.etyp || is_numeric_type e.etyp || e.etyp = T_any ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow >>$? fun e' flow ->
      man.post ~zone:Z_u_num (mk_assume (to_compare_expr e') stmt.srange) flow |>
      OptionExt.return

    | S_expand(e,el) when is_c_num_type e.etyp &&
                          List.for_all (fun ee -> is_c_num_type ee.etyp) el
      ->
      man.eval e ~zone:(Z_c_scalar,Z_u_num) flow >>$? fun e' flow ->
      bind_list el (man.eval ~zone:(Z_c_scalar,Z_u_num)) flow >>$? fun el' flow ->
      man.post (mk_expand e' el' stmt.srange) ~zone:Z_u_num flow |>
      OptionExt.return

    | S_fold(e,el) when is_c_num_type e.etyp &&
                          List.for_all (fun ee -> is_c_num_type ee.etyp) el
      ->
      man.eval e ~zone:(Z_c_scalar,Z_u_num) flow >>$? fun e' flow ->
      bind_list el (man.eval ~zone:(Z_c_scalar,Z_u_num)) flow >>$? fun el' flow ->
      man.post (mk_fold e' el' stmt.srange) ~zone:Z_u_num flow |>
      OptionExt.return

    | S_forget v when is_c_num_type v.etyp ->
      let vv = mk_num_var_expr v in
      man.post (mk_forget vv stmt.srange) ~zone:Z_u_num flow |>
      OptionExt.return

    | _ -> None


  let ask _ _ _ =
    None

  let init _ _ flow =  flow

end

let () =
  Framework.Core.Sig.Stacked.Stateless.register_stack (module Domain)
