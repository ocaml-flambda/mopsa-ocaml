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

(** Symbolic rewriting of expressions as sum, product or quotient of linear forms *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Bot
open Common.Alarms
open Machine_numbers.Domain

module IntItv = ItvUtils.IntItv
module IntBound = ItvUtils.IntBound

module Domain =
struct

  (** {2 Domain header} *)
  (** ================= *)

  include GenStatelessDomainId(struct
      let name = "c.memory.symbolic.rewriting"
    end)

  let checks = [ CHK_C_INTEGER_OVERFLOW;
                 CHK_C_DIVIDE_BY_ZERO;
                 CHK_C_INVALID_SHIFT;
               ]

  let debug_channel = "c.symbolic_rewriting"
  let debug fmt = Debug.debug ~channel:debug_channel fmt


  (** Command-line options *)
  (** ==================== *)

  let () =
    import_shared_option "c.memory.scalars.machine_numbers" name

  
  (** Rewriting environment *)
  type ('a,'b) env = {
    iota: expr -> IntItv.t_with_bot; (* returns an over-approximation of the values of integer expressions *)
    is_in: expr -> IntItv.t -> bool; (* check if an expression is within the bounds of a given interval *)
    man: ('a,'b) man;
    range: range; (* range to use for all rewritten expressions *)
  }

  type linear_form = {
    constant: Z.t;
    coeffs: Z.t VarMap.t;
  }

  type abstract_binop =
    | Add
    | Mult
    | Div
    | ConvexJoin

  (** Abstract expression that is either a linear form, or sum, product, quotient or convex join of abstract expressions *)
  type a_expr =
    | LinearForm of linear_form
    | BinExpr of abstract_binop * a_expr * a_expr

  (** The modular ring in which an expression has to be interpreted, the upper-bound of the ring is excluded *)
  type modulo = NoMod | Mod of Z.t * Z.t

  (** Exception raised when the expression cannot be rewritten using this domain *)
  exception No_representation

  let is_zero _ c = Z.equal Z.zero c

  let one_inf: IntItv.t = (IntBound.Finite Z.one, IntBound.PINF)
  (** [1,+∞] *)

  let minf_mone: IntItv.t = (IntBound.PINF, IntBound.Finite Z.minus_one)
  (** [-∞,-1] *)

  let itv_included_no_bot a b =
    match a with BOT -> false | Nb a -> IntItv.included a b

  let pp_mod fmt = function
    | NoMod -> Format.fprintf fmt "ℤ"
    | Mod (l,u) -> Format.fprintf fmt "[%a,%a[" Z.pp_print l Z.pp_print u

  (** Are two abstract expressions equal. In particular, variables coefficiented by [0] are discarded *)
  let rec abstract_equal = function
    | (LinearForm lf1, LinearForm lf2) ->
      Z.equal lf1.constant lf2.constant &&
      VarMap.for_all2zo is_zero is_zero (fun _ -> Z.equal) lf1.coeffs lf2.coeffs
    | BinExpr (op1, aexpr11, aexpr12), BinExpr (op2, aexpr21, aexpr22) ->
      op1 = op2 && abstract_equal (aexpr11,aexpr21) && abstract_equal (aexpr12,aexpr22)
    | _ -> false

  (** Transform back abstract expressions into integer expressions *)
  let rec to_expr env aexpr = match aexpr with
    | LinearForm { constant; coeffs } ->
      let e = VarMap.fold (fun v coeff e ->
        let v = mk_var v env.range in
        let to_add =
          if Z.equal coeff Z.zero then
            None
          else if Z.equal coeff Z.one then
            Some v
          else if Z.equal coeff Z.minus_one then
            Some (mk_unop ~etyp:T_int O_minus v env.range)
          else
            Some (mk_binop ~etyp:T_int (mk_constant ~etyp:T_int (C_int coeff) env.range) O_mult v env.range)
        in
        (* we try to use as few operators as possible. In particular we avoid adding unnecessary 0 as constants *)
        match e, to_add with
        | None, None -> None
        | None, Some e
        | Some e, None -> Some e
        | Some {ekind=E_unop (O_minus, e1)}, Some {ekind=E_unop (O_minus, e2)} ->
          Some (mk_unop ~etyp:T_int O_minus (mk_binop ~etyp:T_int e1 O_plus e2 env.range) env.range)
        | Some e1, Some {ekind=E_unop (O_minus, e2)}
        | Some {ekind=E_unop (O_minus, e2)}, Some e1 ->
          Some (mk_binop ~etyp:T_int e1 O_minus e2 env.range)
        | Some e1, Some e2 ->
          Some (mk_binop ~etyp:T_int e1 O_plus e2 env.range)
      ) coeffs None
      in
      (* we bring out (towards the root of the AST) the constant so it does not interfere with variable difference patterns *)
      begin match e with
      | None -> mk_constant ~etyp:T_int (C_int constant) env.range
      | Some e when Z.equal Z.zero constant -> e
      | Some e -> mk_binop ~etyp:T_int e O_plus (mk_constant ~etyp:T_int (C_int constant) env.range) env.range
      end
    | BinExpr (ConvexJoin, LinearForm lf1, LinearForm lf2) when VarMap.for_all is_zero lf1.coeffs && VarMap.for_all is_zero lf2.coeffs ->
      mk_constant ~etyp:T_int (C_int_interval (IntBound.Finite (Z.min lf1.constant lf2.constant), IntBound.Finite (Z.max lf1.constant lf2.constant))) env.range
    | BinExpr (abinop, e1, e2) ->
      let binop = match abinop with
        | Add -> O_plus
        | Mult -> O_mult
        | Div -> O_div
        | ConvexJoin -> O_convex_join
      in
      mk_binop ~etyp:T_int (to_expr env e1) binop (to_expr env e2) env.range

  (** Return [Some a] if an abstract expression is the constant [a], otherwise return [None] *)
  let is_constant = function
    | LinearForm { constant; coeffs } when VarMap.for_all is_zero coeffs ->
      Some constant
    | _ -> None

  exception ShouldExit

  (** Return [Some v] if an abstract expression is the variable [v], otherwise return [None] *)
  let is_var = function
    | LinearForm { constant; coeffs } ->
      if not (Z.equal constant Z.zero) then None else
      let result = ref None in
      begin try
        VarMap.iter (fun v c ->
          if Z.equal c Z.one then
            if !result = None then
              result := Some v
            else
              raise ShouldExit
          else if not (Z.equal c Z.zero) then
            raise ShouldExit
        ) coeffs;
        !result
      with ShouldExit -> None
      end
    | _ -> None

  (** Is an abstract expression a difference of variables multiplied by a non-zero constant (e.g., [k(X-A)]).
      In such case, the tuple returned is [(k, X, A)] *)
  let is_difference = function
    | LinearForm { constant; coeffs } ->
      if not (Z.equal constant Z.zero) then None else
      let result = ref None in
      begin try
        VarMap.iter (fun v c ->
          if Z.gt c Z.zero then
            match !result with
            | None ->
              result := Some (c, Some v, None)
            | Some (k, None, Some vneg) when Z.equal k c ->
              result := Some (k, Some v, Some vneg)
            | _ -> raise ShouldExit
          else if Z.lt c Z.zero then
            match !result with
            | None ->
              result := Some (Z.neg c, None, Some v)
            | Some (k, Some vpos, None) when Z.equal k (Z.neg c) ->
              result := Some (k, Some vpos, Some v)
            | _ -> raise ShouldExit
        ) coeffs;
        match !result with
        | Some (k, Some vpos, Some vneg) -> Some (k, vpos, vneg)
        | _ -> None
      with ShouldExit -> None
      end
    | _ -> None

  (** Is an abstract expression representic a deterministic expression *)
  let rec is_deterministic (e: a_expr): bool = match e with
    | LinearForm _ -> true
    | BinExpr (ConvexJoin, _, _) -> false
    | BinExpr ((Add|Mult|Div), e1, e2) ->
      is_deterministic e1 && is_deterministic e2

  (** Return an abstract expression that represents the given variable *)
  let lf_var v =
    LinearForm { constant = Z.zero; coeffs = VarMap.singleton v Z.one }

  (** Return an abstract expression that represents the given constant *)
  let lf_const c =
    LinearForm { constant = c; coeffs = VarMap.empty }

  (** Apply a modular ring evaluation to a constant *)
  let apply_mod c = function
    | NoMod -> c
    | Mod (l,u) -> Z.add l (Z.erem (Z.sub c l) (Z.sub u l))

  (** Can a modular ring [m] be split in [k] sets of same cardinality *)
  let is_m_k_splittable k m =
    match m with
    | NoMod -> true
    | Mod (l,u) -> Z.divisible (Z.sub u l) k

  (** [check_int_overflow env cexp (aexpr, m, flow)] checks whether the C expression
      [cexp] produces an integer overflow and transforms its abstract counterpart
      [(aexpr,m)] accordingly *)
  let rec check_int_overflow env cexp (aexpr, m, flow) =
    let typ = cexp.etyp in
    (* Function that performs the actual check *)
    let do_check ?(exp=cexp) raise_alarm =
      let rmin, rmax = rangeof typ flow in
      let ritv = IntItv.of_z rmin rmax in
      let (aexpr',m') = match m with
        | _ when is_m_k_splittable (Z.sub rmax rmin |> Z.succ) m -> (aexpr, Mod (rmin, Z.succ rmax)) (* rule ModIdentity *)
        | Mod (l,u) ->
          let alpha = apply_mod l (Mod (rmin, Z.succ rmax)) in
          if Z.leq (Z.sub (Z.add alpha u) l) (Z.succ rmax) && Z.divisible (Z.sub alpha l) (Z.sub u l) then
            (aexpr, Mod (alpha, Z.sub (Z.add alpha u) l)) (* rule ModTranslation *)
          else
            (rm_mod env (aexpr, m), Mod (rmin, Z.succ rmax)) (* rule ModIdentityNoMod *)
        | _ -> assert false
      in
      let nexp = to_expr env aexpr' in

      (* the possible wrap-around is kept separate and will be applied latter *)
      if env.is_in nexp ritv then
        (* try to remove the modulo as soon as possible because [ritv] is included into the possible values of [nexp] *)
        let m' = match m' with
          | Mod (l,u) when IntItv.included ritv (Finite l, Finite (Z.pred u)) -> NoMod
          | _ -> m'
        in
        let flow' = safe_c_integer_overflow_check cexp.erange env.man flow in
        (aexpr', m', flow')
      else
        let itv = env.iota nexp in
        let flow' =
          if raise_alarm
          then
            if IntItv.meet_bot itv (Nb ritv) = BOT then
              raise_c_integer_overflow_alarm ~warning:false exp nexp typ cexp.erange env.man flow flow
            else
              raise_c_integer_overflow_alarm ~warning:true exp nexp typ cexp.erange env.man flow flow
          else flow
        in
        (aexpr', m', flow')
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

    | _ -> panic_at cexp.erange "check_int_overflow(symbolic_rewriting): unsupported expression %a" pp_expr cexp


  (** Try to remove the modulo from an abstract expression. Otherwise, raise [No_representation] *)
  and rm_mod env = function
    | (aexpr, NoMod) ->
      aexpr
    | (LinearForm { constant; coeffs }, m) when VarMap.for_all is_zero coeffs ->
      lf_const (apply_mod constant m)
    | (aexpr, Mod (l,u)) ->
      if env.is_in (to_expr env aexpr) (Finite l, Finite (Z.pred u)) then
        aexpr
      else
        let () = if Debug.can_print debug_channel then
          let nexp = to_expr env aexpr in
          let int = env.iota nexp in
          debug "%a is included in %a and failed to be proven in %a" pp_expr nexp IntItv.fprint_bot int pp_mod (Mod (l,u))
        in
        raise No_representation

  (** Given an abstract expression, return its opposite *)
  and opposite env = function
    | LinearForm { constant; coeffs } ->
      LinearForm { constant = Z.neg constant; coeffs = VarMap.map Z.neg coeffs }
    | BinExpr (ConvexJoin, aexpr1, aexpr2) ->
      BinExpr (ConvexJoin, opposite env aexpr2, opposite env aexpr1)
    | BinExpr (Add, aexpr1, aexpr2) ->
      BinExpr (Add, opposite env aexpr1, opposite env aexpr2)
    | BinExpr ((Mult|Div) as op, aexpr1, aexpr2) ->
      BinExpr (op, opposite env aexpr1, aexpr2)

  (** Translates an integer expression into an abstract expression *)
  and abstract (env: ('a,'b) env) (exp: expr) flow =
    match ekind exp with
    (* 𝔼⟦ n ⟧ *)
    | E_constant(C_int c) ->
      (lf_const c, NoMod, flow)

    (* 𝔼⟦ [l,u] ⟧ *)
    | E_constant(C_int_interval (Finite l, Finite u)) when is_c_int_type exp.etyp ->
      let aexpr = reduce env (BinExpr (ConvexJoin, lf_const l, lf_const u)) in
      (aexpr, NoMod, flow)

    (* 𝔼⟦ 'c' ⟧ *)
    | E_constant(C_c_character (c, _)) when is_c_int_type exp.etyp ->
      (lf_const c, NoMod, flow)

    (* 𝔼⟦ var ⟧ *)
    | E_var (v,_) when is_c_int_type v.vtyp ->
      let v = mk_num_var v in
      (* impossible to do constant propagation because lhs of assignment are also translated *)
      (lf_var v, NoMod, flow)

    (* 𝔼⟦ ⋄ e ⟧, ⋄ ∈ {+, -} and type(t) = int *)
    | E_unop((O_plus | O_minus) as unop, e) when exp |> etyp |> is_c_int_type ->
      let (aexpr, m', flow) = abstract env e flow in
      let (aexpr',m') = match unop, m' with
        | O_plus, _ -> (aexpr, m')
        | O_minus, NoMod -> (reduce env (opposite env aexpr), NoMod)
        | O_minus, Mod (l,u) -> (reduce env (opposite env aexpr), Mod (Z.succ (Z.neg u), Z.succ (Z.neg l)))
        | _ -> assert false
      in
      check_int_overflow env exp (aexpr', m', flow)

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {+, -} and type(exp) = int *)
    | E_binop((O_plus|O_minus) as op, e, e') when exp |> etyp |> is_c_int_type ->
      let (aexpr1, m1, flow) = abstract env e flow in
      let (aexpr2, m2, flow) = abstract env e' flow in
      let (aexpr2, m2) = match op, m2 with
        | O_minus, NoMod -> (reduce env (opposite env aexpr2), m2)
        | O_minus, Mod (l,u) -> (reduce env (opposite env aexpr2), Mod (Z.succ (Z.neg u), Z.succ (Z.neg l)))
        | _ -> (aexpr2, m2)
      in
      let (aexpr, m) =
        match (is_constant aexpr1, aexpr1, m1), (is_constant aexpr2, aexpr2, m2) with
        | (Some alpha1, _, m1), (Some alpha2, _, m2) ->
          (lf_const (Z.add (apply_mod alpha1 m1) (apply_mod alpha2 m2)), NoMod)
        | (Some alpha, _, m_alpha), (None, aexpr, Mod (l,u))
        | (None, aexpr, Mod (l,u)), (Some alpha, _, m_alpha) ->
          let alpha' = apply_mod alpha m_alpha in
          (BinExpr (Add, lf_const alpha', aexpr)), Mod (Z.add l alpha', Z.add u alpha')
        | _ ->
          let rmin, rmax = rangeof exp.etyp flow in
          let rlen = Z.succ (Z.sub rmax rmin) in
          if is_m_k_splittable rlen m1 && is_m_k_splittable rlen m2 then
            (BinExpr (Add, aexpr1, aexpr2)), Mod (rmin, Z.succ rmax)
          else
            (BinExpr (Add, rm_mod env (aexpr1, m1), rm_mod env (aexpr2, m2)), NoMod)
      in
      check_int_overflow env exp (reduce env aexpr, m, flow)

      (* 𝔼⟦ e * e' ⟧ and type(exp) = int *)
    | E_binop(O_mult, e, e') when exp |> etyp |> is_c_int_type ->
      let (aexpr1, m1, flow) = abstract env e flow in
      let (aexpr2, m2, flow) = abstract env e' flow in
      let (aexpr, m) =
        match (is_constant aexpr1, aexpr1, m1), (is_constant aexpr2, aexpr2, m2) with
        | (Some alpha1, _, m1), (Some alpha2, _, m2) ->
          (lf_const (Z.mul (apply_mod alpha1 m1) (apply_mod alpha2 m2)), NoMod)
        | (Some alpha, _, m_alpha), (None, aexpr, Mod (l,u))
        | (None, aexpr, Mod (l,u)), (Some alpha, _, m_alpha) ->
          let alpha' = apply_mod alpha m_alpha in
          if Z.lt Z.zero alpha' then
            (BinExpr (Mult, aexpr, lf_const alpha'), Mod (Z.mul l alpha', Z.mul u alpha'))
          else if Z.equal Z.zero alpha' then
            (lf_const Z.zero, NoMod)
          else
            (BinExpr (Mult, aexpr, lf_const alpha'), Mod (Z.succ (Z.mul u alpha'), Z.succ (Z.mul l alpha')))
        | _ ->
          let rmin, rmax = rangeof exp.etyp flow in
          let rlen = Z.succ (Z.sub rmax rmin) in
          if is_m_k_splittable rlen m1 && is_m_k_splittable rlen m2 then
            (BinExpr (Mult, aexpr1, aexpr2), Mod (rmin, Z.succ rmax))
          else
            (BinExpr (Mult, rm_mod env (aexpr1, m1), rm_mod env (aexpr2, m2)), NoMod)
      in
      check_int_overflow env exp (reduce env aexpr, m, flow)

      (* 𝔼⟦ e / e' ⟧, type(exp) = int *)
    | E_binop(O_div, e, e') when exp |> etyp |> is_c_int_type ->
      let (aexpr1, m1, flow) = abstract env e flow in
      let (aexpr2, m2, flow) = abstract env e' flow in
      let nexp2 = (aexpr2, m2) |> rm_mod env |> to_expr env in
      let flow =
        if env.is_in nexp2 IntItv.zero then
          raise No_representation (* first approximation *)
        else
          Flow.add_safe_check CHK_C_DIVIDE_BY_ZERO exp.erange flow
      in
      let (aexpr, m) =
        match is_constant aexpr2, m1 with
        | Some alpha, Mod (l,u) ->
          let aexpr1_positive = env.is_in (to_expr env aexpr1) IntItv.zero_inf in
          let alpha' = apply_mod alpha m2 in
          if aexpr1_positive && Z.lt Z.zero alpha' && Z.leq Z.zero l && Z.divisible l alpha' && Z.divisible u alpha' then
            (BinExpr (Div, aexpr1, lf_const alpha'), Mod (Z.divexact l alpha', Z.divexact u alpha'))
          else if aexpr1_positive && Z.gt Z.zero alpha' && Z.gt Z.zero u && Z.divisible (Z.succ u) alpha' && Z.divisible (Z.succ l) alpha' then
            (BinExpr (Div, aexpr1, lf_const alpha'), Mod (Z.divexact (Z.succ u) alpha', Z.divexact (Z.succ l) alpha'))
          else
            (BinExpr (Div, rm_mod env (aexpr1, m1), rm_mod env (aexpr2, m2)), NoMod)
        | _ ->
          (BinExpr (Div, rm_mod env (aexpr1, m1), rm_mod env (aexpr2, m2)), NoMod)
      in
      check_int_overflow env exp (reduce env aexpr, m, flow)

    (* 𝔼⟦ e ⋓ e' ⟧, ⋄ ∈ {+, -, *} and type(exp) = int *)
    | E_binop(O_convex_join, e, e') when exp |> etyp |> is_c_int_type ->
      let (aexpr1, m1, flow) = abstract env e flow in
      let (aexpr2, m2, flow) = abstract env e' flow in
      let aexpr1' = rm_mod env (aexpr1, m1) in
      let aexpr2' = rm_mod env (aexpr2, m2) in
      let aexpr = reduce env (BinExpr (ConvexJoin, aexpr1', aexpr2')) in
      (aexpr, NoMod, flow)

    (* 𝔼⟦ (int)int ⟧ *)
    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type && e |> etyp |> is_c_int_type ->
      abstract env e flow |>
      check_int_overflow env exp

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {>>, <<} *)
    | E_binop(op, e, e') when op |> is_c_shift_op && exp |> etyp |> is_c_int_type ->
      let (aexpr2, m2, flow) = abstract env e' flow in
      let aexpr2' = rm_mod env (aexpr2, m2) in
      (* when the shift is *by a constant* and is safe, transform it in a product or a quotient *)
      begin match env.iota (to_expr env aexpr2') with
      | Nb (Finite l, Finite u) when Z.equal l u ->
        (* Condition: l ∈ [0, bits(t) - 1] *)
        let bits = sizeof_type exp.etyp flow |> Z.mul (Z.of_int 8) in
        if Z.leq Z.zero l && Z.lt l bits then
          let flow = safe_c_shift_check exp.erange env.man flow in
          let e' = mk_z ~typ:T_int (l |> Z.to_int |> Z.shift_left Z.one) env.range in
          let op' = match op with
            | O_bit_lshift -> O_mult
            | O_bit_rshift -> O_div
            | _ -> assert false
          in
          let exp' = { exp with ekind = E_binop(op', e, e') } in
          abstract env exp' flow
        else
          raise No_representation
      | _ -> raise No_representation
      end

    | _ -> raise No_representation

  (** Try to reduce the number of nodes of abstract expressions *)
  and [@warning "-57"] reduce env e =
    match e with
    | BinExpr (ConvexJoin, e1, e2) ->
      begin match e1, e2 with
      (* e1 [U] e1 = e1 *)
      | _ when is_deterministic e1 && abstract_equal (e1,e2) ->
        e1
      | (BinExpr (ConvexJoin, e11, e12), BinExpr (ConvexJoin, e21, e22)) ->
        (* (e1 [U] e2) [U] (e1 [U] e3) = (e3 [U] e2) [U] (e1 [U] e3) = e2 [U] (e1 [U] e3) *)
        if is_deterministic e11 && (abstract_equal (e11,e21) || abstract_equal (e11,e22)) then
          reduce env (BinExpr (ConvexJoin, e12, e2))
        (* (e1 [U] e2) [U] (e2 [U] e3) = (e1 [U] e3) [U] (e2 [U] e3) = e1 [U] (e2 [U] e3) *)
        else if is_deterministic e12 && (abstract_equal (e12,e21) || abstract_equal (e12,e22)) then
          reduce env (BinExpr (ConvexJoin, e11, e2))
        else
          e
      | (e1, BinExpr (ConvexJoin, e2, e3))
      | (BinExpr (ConvexJoin, e2, e3), e1) ->
        (* e1 [U] (e1 [U] e2) = (e1 [U] e2) [U] e1 = e1 [U] e2 *)
        if is_deterministic e1 && (abstract_equal (e1,e2) || abstract_equal (e1,e3)) then
          BinExpr (ConvexJoin, e2, e3)
        else
          e
      | _ -> e
      end
    (* addition of linear forms *)
    | BinExpr (Add, LinearForm lf1, LinearForm lf2) ->
      LinearForm {
        constant = Z.add lf1.constant lf2.constant;
        coeffs = VarMap.map2o (fun _ -> Fun.id) (fun _ -> Fun.id) (fun _ -> Z.add) lf1.coeffs lf2.coeffs;
      }
    (* 0 + e1 = e1 + 0 = e1 *)
    | BinExpr (Add, e1, e2) when is_constant e1 = Some Z.zero ->
      e2
    | BinExpr (Add, e1, e2) when is_constant e2 = Some Z.zero ->
      e1
    (* application of operation by deterministic expression on both sides of a convex join *)
    | BinExpr ((Add|Mult|Div) as op, BinExpr (ConvexJoin, e1, e2), e3)
    | BinExpr ((Add|Mult) as op, e3, BinExpr (ConvexJoin, e1, e2))
        when is_deterministic e3 ->
      reduce env (BinExpr (ConvexJoin, reduce env (BinExpr (op, e1, e3)), reduce env (BinExpr (op, e2, e3))))
    (* simplify addition or multiplication of convex joins with 0 when other operands have the same sign *)
    | BinExpr ((Add|Mult) as op, BinExpr (ConvexJoin, e11, e12), BinExpr (ConvexJoin, e21, e22)) ->
      let res1 =
        if is_constant e11 = Some Z.zero then Some e12 else
        if is_constant e12 = Some Z.zero then Some e11 else
        None
      in
      let res2 =
        if is_constant e21 = Some Z.zero then Some e22 else
        if is_constant e22 = Some Z.zero then Some e21 else
        None
      in
      begin match res1, res2 with
      | Some e1, Some e2 ->
        let int1 = env.iota (to_expr env e1) in
        let int2 = env.iota (to_expr env e2) in
        if itv_included_no_bot int1 IntItv.zero_inf && itv_included_no_bot int2 IntItv.zero_inf then
          reduce env (BinExpr (ConvexJoin, lf_const Z.zero, reduce env (BinExpr (op, e1, e2))))
        else if itv_included_no_bot int1 IntItv.minf_zero && itv_included_no_bot int2 IntItv.minf_zero then
          reduce env (BinExpr (ConvexJoin, lf_const Z.zero, reduce env (BinExpr (op, e1, e2))))
        else e
      | _ -> e
      end
    (* multiplication of a linear form by a constant *)
    | BinExpr (Mult, LinearForm lf1, LinearForm lf2) when
        VarMap.for_all is_zero lf1.coeffs ->
      LinearForm {
        constant = Z.mul lf2.constant lf1.constant;
        coeffs = VarMap.map (Z.mul lf1.constant) lf2.coeffs;
      }
    | BinExpr (Mult, LinearForm lf1, LinearForm lf2) when
        VarMap.for_all is_zero lf2.coeffs ->
      LinearForm {
        constant = Z.mul lf1.constant lf2.constant;
        coeffs = VarMap.map (Z.mul lf2.constant) lf1.coeffs;
      }
    (* multiplication by 1 or -1 *)
    | (BinExpr (Mult, LinearForm lf1, e2)
    | BinExpr (Mult, e2, LinearForm lf1)) when
        VarMap.for_all is_zero lf1.coeffs ->
      if Z.equal lf1.constant Z.one then
        e2
      else if Z.equal lf1.constant Z.minus_one then
        opposite env e2
      else
        e
    | BinExpr (Div, LinearForm lf1, LinearForm lf2) when
        lf2.constant <> Z.zero &&
        VarMap.for_all is_zero lf2.coeffs &&
        VarMap.for_all (fun _ c -> Z.divisible c lf2.constant) lf1.coeffs ->
      LinearForm {
        constant = Z.divexact lf1.constant lf2.constant;
        coeffs = VarMap.map (fun c -> Z.divexact c lf2.constant) lf1.coeffs;
      }
      (* division by 1 or -1 *)
    | BinExpr (Div, e1, LinearForm lf2) when
        VarMap.for_all is_zero lf2.coeffs &&
        Z.equal Z.one (Z.abs lf2.constant) ->
      if Z.equal lf2.constant Z.one then
        e1
      else if Z.equal lf2.constant Z.minus_one then
        opposite env e1
      else
        assert false
    | BinExpr (Div, e1, e2) ->
      let res = ref None in
      
      (* 0/e2 *)
      begin match is_constant e1 with
      | Some k when Z.equal Z.zero k -> res := Some (lf_const Z.zero)
      | _ -> ()
      end;

      (* (X-A)*ez / (B-A) *)
      begin if !res = None then match is_difference e2 with
      | Some (k2, o, p) ->

        let store_res_if_interpolation_pattern_2 var_diff ez =
          match var_diff with
          | None -> false
          | Some (k1, m, n) ->
            if Z.divisible k1 k2 then begin
              let vars =
                if n = p then Some (p, o, m) (* either (X-A)/(B-A) *)
                else if m = o then Some (p, o, n) (* or (B-X)/(B-A) *)
                else None
              in
              match vars with
              | Some (a, b, x) when
                  itv_included_no_bot (env.iota (mk_binop ~etyp:T_int (mk_var x env.range) O_minus (mk_var a env.range) env.range)) IntItv.zero_inf &&
                  itv_included_no_bot (env.iota (mk_binop ~etyp:T_int (mk_var b env.range) O_minus (mk_var x env.range) env.range)) IntItv.zero_inf &&
                  itv_included_no_bot (env.iota (mk_binop ~etyp:T_int (mk_var b env.range) O_minus (mk_var a env.range) env.range)) one_inf ->
                let ez' = reduce env (BinExpr (Mult, lf_const (Z.divexact k1 k2), ez)) in

                (* if X<B and the result is named R, then |R| <= |ez'| - 1
                  (if ez'=0 it goes well because 0 is added afterward via the convex join) *)
                let ez' =
                  if itv_included_no_bot (env.iota (mk_binop ~etyp:T_int (mk_var b env.range) O_minus (mk_var x env.range) env.range)) one_inf
                  then
                    let int = env.iota (to_expr env ez') in
                    if itv_included_no_bot int (Finite Z.zero, Finite Z.zero) then
                      lf_const Z.zero
                    else if itv_included_no_bot int IntItv.zero_inf then
                      reduce env (BinExpr (Add, lf_const Z.minus_one, ez'))
                    else if itv_included_no_bot int IntItv.minf_zero then
                      reduce env (BinExpr (Add, lf_const Z.one, ez'))
                    else
                      ez'
                  else ez'
                in

                res := Some (reduce env (BinExpr (ConvexJoin, lf_const Z.zero, ez')));
                true
              | _ -> false
            end
            else
              false
        in

        begin match e1 with
        | BinExpr (Mult, e11, e12) when store_res_if_interpolation_pattern_2 (is_difference e11) e12 -> ()
        | BinExpr (Mult, e11, e12) when store_res_if_interpolation_pattern_2 (is_difference e12) e11 -> ()
        | _ when store_res_if_interpolation_pattern_2 (is_difference e1) (lf_const Z.one) -> ()
        | _ -> ()
        end
      | _ -> ()
      end;

      (* (X-A)*ez / alpha *)
      begin if !res = None then match is_constant e2 with
      | Some d ->

        let store_res_if_interpolation_pattern_1 var_diff ez =
          match var_diff with
          | None -> false
          | Some (k, x, a) ->
            begin match env.iota (mk_binop ~etyp:T_int (mk_var x env.range) O_minus (mk_var a env.range) env.range) with
            | Nb (Finite mi, Finite ma) ->
              let (kmi,kma) =
                if Z.leq Z.zero k
                then (Z.mul k mi, Z.mul k ma)
                else (Z.mul k ma, Z.mul k mi)
              in
              let (mi',mi'_exact,ma',ma'_exact) =
                if Z.leq Z.zero d
                then (Z.fdiv kmi d, Z.divisible kmi d, Z.cdiv kma d, Z.divisible kma d)
                else (Z.fdiv kma d, Z.divisible kma d, Z.cdiv kmi d, Z.divisible kmi d)
              in

              let to_add =
                let int = env.iota (to_expr env ez) in
                if itv_included_no_bot int one_inf then
                  Some Z.one
                else if itv_included_no_bot int minf_mone then
                  Some Z.minus_one
                else
                  None
              in

              let res1 = reduce env (BinExpr (Mult, ez, lf_const mi')) in
              let res1 =
                match to_add with
                | Some to_add when Z.gt Z.zero mi' && not mi'_exact ->
                  reduce env (BinExpr (Add, res1, lf_const to_add))
                | _ -> res1
              in

              let res2 = reduce env (BinExpr (Mult, ez, lf_const ma')) in
              let res2 =
                match to_add with
                | Some to_add when Z.lt Z.zero ma' && not ma'_exact ->
                  reduce env (BinExpr (Add, res2, lf_const (Z.neg to_add)))
                | _ -> res2
              in

              res := Some (reduce env (BinExpr (ConvexJoin, res1, res2)));
              true
            | _ -> false
            end
        in

        begin match e1 with
        | BinExpr (Mult, e11, e12) when store_res_if_interpolation_pattern_1 (is_difference e11) e12 -> ()
        | BinExpr (Mult, e11, e12) when store_res_if_interpolation_pattern_1 (is_difference e12) e11 -> ()
        | _ when store_res_if_interpolation_pattern_1 (is_difference e1) (lf_const Z.one) -> ()
        | _ -> ()
        end
      | _ -> ()
      end;

      (* finally return the simplified expression or act as the identity *)
      begin match !res with
      | Some res -> res
      | None -> e
      end
    | _ -> e

  (** Are the operators of a given expression supported by the [abstract] function *)
  let rec is_supported_expr exp = is_c_int_type exp.etyp && match ekind exp with
    | E_constant(C_int _ | C_int_interval (Finite _, Finite _) | C_c_character _) ->
      true
    | E_var (v,_) ->
      is_c_int_type v.vtyp
    | E_unop((O_plus | O_minus), e)
    | E_c_cast(e, _) ->
      is_supported_expr e
    | E_binop((O_plus | O_minus | O_mult | O_div | O_convex_join | O_bit_lshift | O_bit_rshift), e, e') ->
      is_supported_expr e && is_supported_expr e'
    | _ -> false

  (** Translate comparison of integer expressions into comparison to [0],
      otherwise call [abstract] and try to remove the modular ring in which the abstract expression is evaluated.
      If this fails, [No_representation] is raised.  *)
  let abstract_with_comparisons env exp flow =
    match ekind exp with
      (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {<, <=, >, >=, ==, !=} *)
      | E_binop((O_gt | O_ge | O_lt | O_le | O_eq | O_ne) as comp_op, e, e') when is_supported_expr e && is_supported_expr e'->
        let (aexpr1, m1, flow) = abstract env e flow in
        let (aexpr2, m2, flow) = abstract env e' flow in
        let zero = mk_int 0 ~typ:T_int env.range in
        begin match m1, m2 with
        | Mod (l1,u1), Mod (l2,u2) when
            Z.equal l2 (Z.succ (Z.neg u1)) &&
            Z.equal u2 (Z.succ (Z.neg l1)) &&
            is_constant (reduce env (BinExpr (Add, aexpr1, aexpr2))) = Some Z.zero ->
          (mk_binop ~etyp:T_int zero comp_op zero env.range, flow) (* rule PlusEqZero *)
        | _ ->
          let aexpr = reduce env (BinExpr (Add, rm_mod env (aexpr1, m1), opposite env (rm_mod env (aexpr2, m2)))) in
          (mk_binop ~etyp:T_int (to_expr env aexpr) comp_op zero env.range, flow)
        end
      | _ when is_supported_expr exp ->
        let (aexpr,m,flow') = abstract env exp flow in
        let e' = to_expr env (rm_mod env (aexpr, m)) in
        (e', flow')
      | _ ->
        raise No_representation


  (** {2 Transfer functions} *)
  (** ====================== *)

  let eval exp (man: ('a,'b) man) flow =
    match exp.etyp with
    | T_c_integer _ ->

      (* rewriting environment *)
      let iota (e: expr): IntItv.t_with_bot =
        try
          ask_and_reduce man.ask (Universal.Numeric.Common.mk_int_interval_query ~fast:false e) flow
        with Not_found ->
          let () = debug "Couldn't query interval of %a, the backtrace is:" pp_expr e in
          let () = if Debug.can_print debug_channel then Printexc.print_backtrace stdout in
          raise No_representation
      in
      let is_in (e: expr) (int: IntItv.t): bool =
        try
          if itv_included_no_bot (ask_and_reduce man.ask (Universal.Numeric.Common.mk_int_interval_query ~fast:true e) flow) int then
            true
          else
            itv_included_no_bot (ask_and_reduce man.ask (Universal.Numeric.Common.mk_int_interval_query ~fast:false e) flow) int
        with Not_found ->
          let () = debug "Couldn't query interval of %a, the backtrace is:" pp_expr e in
          let () = if Debug.can_print debug_channel then Printexc.print_backtrace stdout in
          raise No_representation
      in
      let env: ('a,'b) env = {
          iota;
          is_in;
          man;
          range = mk_tagged_range (String_tag debug_channel) exp.erange;
        } in

      begin try
        let (nexp, flow') = abstract_with_comparisons env exp flow in
        let () = debug "Rewritten expression %a into %a." pp_expr exp pp_expr nexp in
        Eval.singleton exp flow' |>
        Eval.add_translation "Universal" nexp |>
        OptionExt.return
      with No_representation ->
        let () = debug "Failed to rewrite %a, the backtrace is:" pp_expr exp in
        let () = if Debug.can_print debug_channel then Printexc.print_backtrace stdout in
        None
      end

    | _ -> None

  let init _ _ flow = flow
  let ask _ _ _ = None
  let exec _ _ _ = None
  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
