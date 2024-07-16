(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(* Author Jérôme Boillot <jerome.boillot@ens.fr>                            *)
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

(** Symbolic rewriting of expressions as sum, product or quotient of linear forms
    Mainly inspired by the article Symbolic transformation of expressions in modular arithmetic SAS'23
*)

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

  let debug_channel = name

  (** Command-line options *)
  (** ==================== *)

  let () =
    import_shared_option "c.memory.scalars.machine_numbers" name;
    import_shared_option "universal.numeric.relational" name

  
  (** Rewriting environment *)
  type ('a,'b) env = {
    iota: expr -> IntItv.t; (* returns an over-approximation of the values of expressions *)
    is_var_constant: var -> Z.t option; (* if a variable is a constant integer, returns its value *)
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

  (** Abstract expression that is either a linear form, or sum, product, quotient or convex join of abstract expressions.
      It can also be delayed operation over an abstract expression when interpreted in a [ToBeKSplit] modular ring. *)
  type a_expr =
    | LinearForm of linear_form
    | BinExpr of abstract_binop * a_expr * a_expr
    | DelayedWrap of a_expr * bool * IntItv.t
      (** [DelayedWrap (e,s,itv)] is the abstract expression [e] wrapped in the interval [itv], with a unary minus if [not s].
          It can appear in an abstract expression only when associated with a [ToBeKSplit] *)
    | DelayedMod of a_expr * bool * Z.t
      (** [DelayedMod (e,s,n)] is the abstract expression [e] evaluated [% n], with a unary minus if [not s].
          It can appear in an abstract expression only when associated with a [ToBeKSplit] *)

  (** The modular ring in which an expression has to be interpreted, the upper-bound of the ring is excluded *)
  type modulo =
    | NoMod
    | Mod of Z.t * Z.t (** [Mod (l,u)] means that the associated abstract expression has to be interpreted in the modular ring \[l,u\[ *)
    | ToBeKSplit of Z.t (** [ToBeKSplit k] means that the current abstract expression cannot be interpreted in a modular ring, but that, if
        it is later interpeted in a modular ring that splits [ℤ_k], it will be exactly the abstract expression intepreted in the latter modular ring.
        Then, and only in that case, abstract expressions can contain [DelayedWrap] and [DelayedMod] that preserve the soundness of the
        intermediate checks, but which will be removed at the same time as the [ToBeKSplit].
        This is used to delay the need for a modular ring in which we interpret the abstract expression (that contains no [Mod]).
        For example, `(uint8_t) ((uint8_t) a + (uint8_t) b + (uint8_t) c) = a + b + c` if it can be represented by an `uint8_t`. *)

  (** Exception raised when the expression cannot be rewritten using this domain *)
  exception No_representation

  let is_zero _ c = Z.equal Z.zero c

  let one_inf: IntItv.t = (IntBound.Finite Z.one, IntBound.PINF)
  (** [1,+∞] *)

  let minf_mone: IntItv.t = (IntBound.PINF, IntBound.Finite Z.minus_one)
  (** [-∞,-1] *)

  let pp_mod fmt = function
    | NoMod -> ()
    | Mod (l,u) -> Format.fprintf fmt " mod [%a,%a[" Z.pp_print l Z.pp_print u
    | ToBeKSplit k -> Format.fprintf fmt " to-%a-split" Z.pp_print k

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
    | DelayedWrap (e1, s, (IntBound.Finite l, IntBound.Finite h)) ->
      let e1' = to_expr env e1 in
      let e1' = wrap_expr e1' (l,h) env.range in
      if s then e1' else mk_unop ~etyp:T_int O_minus e1' env.range
    | DelayedWrap _ -> assert false
    | DelayedMod (e1, s, n) ->
      let e1' = to_expr env e1 in
      let e1' = mk_binop ~etyp:T_int e1' O_mod (mk_z n ~typ:T_int env.range) env.range in
      if s then e1' else mk_unop ~etyp:T_int O_minus e1' env.range

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

  (** Is an abstract expression representing a deterministic expression *)
  let rec is_deterministic (e: a_expr): bool = match e with
    | LinearForm _ -> true
    | BinExpr (ConvexJoin, _, _) -> false
    | BinExpr ((Add|Mult|Div), e1, e2) ->
      is_deterministic e1 && is_deterministic e2
    | DelayedWrap _ | DelayedMod _ -> false

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
    | ToBeKSplit _ -> assert false

  (** Can a modular ring [m] be split in [k] sets of same cardinality *)
  let is_m_k_splittable k m =
    match m with
    | NoMod -> true
    | Mod (l,u) -> Z.divisible (Z.sub u l) k
    | ToBeKSplit k' -> Z.divisible k' k

  (** [check_int_overflow env cexp (aexpr, m, flow)] checks whether the C expression
      [cexp] produces an integer overflow and transforms its abstract counterpart
      [(aexpr',m')] accordingly *)
  let rec check_int_overflow env cexp (aexpr, m, flow) =
    let typ = cexp.etyp in
    (* Function that performs the actual check *)
    let do_check ?(exp=cexp) raise_alarm =
      let rmin, rmax = rangeof typ flow in
      let ritv = IntItv.of_z rmin rmax in

      (* to produce no overflows, the inner expression [aexpr] evaluated in the modular ring [m], called [nexp], has to be within [[rmin,rmax]]... *)
      let (nexp, aexpr, m) =
        try
          let aexpr' = rm_mod env (aexpr, m) in
          (to_expr env aexpr', aexpr', NoMod)
        with No_representation ->
          let nexp = to_expr env aexpr in
          match m with
          | NoMod -> assert false
          | Mod (l,u) -> (wrap_expr nexp (l, Z.pred u) env.range, aexpr, m)
          | ToBeKSplit _ -> (nexp, aexpr, m) (* in this case, all the wraps are already made explicit in [aexpr] *)
      in

      (* ...but, when later evaluated in the modular ring [[rmin,rmax]], [aexpr] can be modified without compromising the soundness of the rewriting. *)
      let (aexpr', m') = apply_mod_aexpr env (aexpr, m) (rmin, Z.succ rmax) in

      (* An example of the importance of the previous simplification order is: `(unsigned char) (x + 256)`.
         This expression can be rewritten as `x mod [0,256[` but only after checking the possible overflow of the addition.
      *)

      let flow =
        if env.is_in nexp ritv then
          safe_c_integer_overflow_check cexp.erange env.man flow
        else
          let itv = env.iota nexp in
          if raise_alarm then
            if IntItv.meet itv ritv = BOT then
              raise_c_integer_overflow_alarm ~warning:false exp nexp typ cexp.erange env.man flow flow
            else
              raise_c_integer_overflow_alarm ~warning:true exp nexp typ cexp.erange env.man flow flow
          else flow
      in
      (aexpr', m', flow)
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
    | (LinearForm { constant; coeffs }, (Mod _ as m)) when VarMap.for_all is_zero coeffs ->
      lf_const (apply_mod constant m)
    | (aexpr, Mod (l,u)) ->
      if env.is_in (to_expr env aexpr) (Finite l, Finite (Z.pred u)) then
        aexpr
      else
        let () = if Debug.can_print debug_channel then
          let nexp = to_expr env aexpr in
          let int = env.iota nexp in
          debug "%a is included in %a and failed to be proven in [%a,%a]" pp_expr nexp IntItv.fprint int Z.pp_print l Z.pp_print (Z.pred u)
        in
        raise No_representation
    | (_, ToBeKSplit _) ->
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
    | DelayedWrap (e1, s, itv) -> DelayedWrap (e1, not s, itv)
    | DelayedMod (e1, s, n) -> DelayedMod (e1, not s, n)

  and opposite_mod_ring m = match m with
    | Mod (l,u) -> Mod (Z.succ (Z.neg u), Z.succ (Z.neg l))
    | NoMod | ToBeKSplit _ -> m

  and remove_delayed env e = match e with
    | LinearForm _ -> e
    | BinExpr (op, e1, e2) ->
      let e1' = remove_delayed env e1 in
      let e2' = remove_delayed env e2 in
      if e1 == e1' && e2 == e2'
      then e
      else reduce env (BinExpr (op, e1', e2'))
    | DelayedWrap (e1, s, _) ->
      let e1' = remove_delayed env e1 in
      if s then e1 else reduce env (opposite env e1')
    | DelayedMod (e1, s, _) ->
      let e1' = remove_delayed env e1 in
      if s then e1 else reduce env (opposite env e1')

  (** Return the cardinal of a non-infinite modular ring *)
  and get_m_cardinal = function
    | NoMod -> assert false
    | Mod (l,u) -> Z.sub u l
    | ToBeKSplit k -> k

  (** Return the modular ring that splits both arguments *)
  and split_both m1 m2 = match m1,m2 with
    | NoMod, NoMod -> NoMod
    | NoMod, m
    | m, NoMod -> ToBeKSplit (get_m_cardinal m)
    | _ -> ToBeKSplit (Z.gcd (get_m_cardinal m1) (get_m_cardinal m2))

  and apply_mod_aexpr env (aexpr, m) (l',u') =
    let rlen = Z.sub u' l' in
    match m with
    | _ when is_m_k_splittable rlen m ->
      let aexpr = match m with
        | ToBeKSplit _ -> remove_delayed env aexpr
        | NoMod | Mod _ -> aexpr
      in 
      (simplify_linear_forms env rlen aexpr, Mod (l',u')) (* rule ModIdentity *)
    | NoMod -> assert false (* handled by rule ModIdentity *)
    | Mod (l,u) when
        let alpha = apply_mod l (Mod (l',u')) in
        Z.leq (Z.sub (Z.add alpha u) l) u' && Z.divisible (Z.sub alpha l) (Z.sub u l) ->
      let alpha = apply_mod l (Mod (l',u')) in
      let aexpr = simplify_linear_forms env (Z.sub u l) aexpr in
      (aexpr, Mod (alpha, Z.add alpha (Z.sub u l))) (* rule ModTranslation *)
    | _ ->
      try
        (rm_mod env (aexpr, m), Mod (l',u')) (* rule ModIdentityNoMod *)
      with No_representation ->
        let ritv = IntItv.of_z l' (Z.pred u') in
        if env.is_in (to_expr env aexpr) ritv then
          (aexpr, m)
        else
          let aexpr' = DelayedWrap (aexpr, true, ritv) in
          (aexpr', ToBeKSplit (Z.gcd rlen (get_m_cardinal m)))

  (** Simplify the coefficients of the linear forms of abstract expressions when interpreted in a modular ring of cardinal [n] within [[-floor((n-1)/2), ceil((n-1)/2)+1]] *)
  and simplify_linear_forms env n e =
    let (nd2,nd2_rem) = Z.ediv_rem (Z.pred n) (Z.of_int 2) in
    let (l,u) = (Z.neg nd2, Z.succ (Z.add nd2 nd2_rem)) in
    let rec aux e = match e with
      | LinearForm { constant; coeffs } ->
        let modified_lf = ref false in
        let f c =
          if Z.leq l c && Z.lt c u then c
          else (modified_lf := true ; apply_mod c (Mod (l,u)))
        in
        let e' = LinearForm {
            constant = f constant;
            coeffs = VarMap.map f coeffs;
          }
        in
        if !modified_lf then e' else e
      | BinExpr ((Add | Mult as op), e1, e2) ->
        let e1' = aux e1 in
        let e2' = aux e2 in
        if e1 == e1' && e2 == e2'
        then e
        else reduce env (BinExpr (op, e1', e2'))
      | e -> e
    in
    aux e

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
      begin match env.is_var_constant v with
      | Some n -> (lf_const n, NoMod, flow)
      | None -> (lf_var v, NoMod, flow)
      end

    (* 𝔼⟦ + e ⟧, type(t) = int *)
    | E_unop(O_plus, e) when exp |> etyp |> is_c_int_type ->
      let (aexpr, m, flow) = abstract env e flow in
      check_int_overflow env exp (aexpr, m, flow)

    (* 𝔼⟦ - e ⟧, type(t) = int *)
    | E_unop(O_minus, e) when exp |> etyp |> is_c_int_type ->
      let (aexpr, m, flow) = abstract env e flow in
      let aexpr' = reduce env (opposite env aexpr) in
      let m' = opposite_mod_ring m in
      check_int_overflow env exp (aexpr', m', flow)

    (* 𝔼⟦ ~ e ⟧, type(t) = int *)
    | E_unop(O_bit_invert, e) when exp |> etyp |> is_c_int_type ->
      let (aexpr, m, flow) = abstract env e flow in
      let aexpr' = reduce env (BinExpr (Add, reduce env (opposite env aexpr), lf_const Z.minus_one)) in
      let m' = match m with
        | NoMod -> NoMod
        | Mod (l,u) -> Mod (Z.neg u, Z.neg l)
        | ToBeKSplit _ as m -> m
      in
      check_int_overflow env exp (aexpr', m', flow)

    (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {+, -} and type(exp) = int *)
    | E_binop((O_plus|O_minus) as op, e, e') when exp |> etyp |> is_c_int_type ->
      let (aexpr1, m1, flow) = abstract env e flow in
      let (aexpr2_pos, m2, flow) = abstract env e' flow in
      let (aexpr2, m2) = match op with
        | O_plus -> (aexpr2_pos, m2)
        | O_minus -> (reduce env (opposite env aexpr2_pos), opposite_mod_ring m2)
        | _ -> assert false
      in
      let (aexpr, m) =
        match (is_constant aexpr1, aexpr1, m1), (is_constant aexpr2, aexpr2, m2) with
        | (Some alpha1, _, (NoMod | Mod _)), (Some alpha2, _, (NoMod | Mod _)) ->
          (lf_const (Z.add (apply_mod alpha1 m1) (apply_mod alpha2 m2)), NoMod)
        | (Some alpha, _, (NoMod|Mod _ as m_alpha)), (None, aexpr, Mod (l,u))
        | (None, aexpr, Mod (l,u)), (Some alpha, _, (NoMod|Mod _ as m_alpha)) ->
          let alpha' = apply_mod alpha m_alpha in
          (BinExpr (Add, lf_const alpha', aexpr)), Mod (Z.add l alpha', Z.add u alpha')
        | (_,_,NoMod), (_,_,NoMod) ->
          (BinExpr (Add, aexpr1, aexpr2), NoMod)
        | _ ->
          try
            (BinExpr (Add, rm_mod env (aexpr1, m1), rm_mod env (aexpr2, m2)), NoMod)
          with No_representation ->
            let rmin1, rmax1 = rangeof e.etyp flow in
            let ritv1 = IntItv.of_z rmin1 rmax1 in
            let aexpr1 = match m1 with
              | NoMod -> aexpr1
              | _ when env.is_in (to_expr env aexpr1) ritv1 -> aexpr1
              | _ -> DelayedWrap (aexpr1, true, ritv1) in
            let rmin2, rmax2 = rangeof e'.etyp flow in
            let ritv2 = IntItv.of_z rmin2 rmax2 in
            let aexpr2 = match m2 with
              | NoMod -> aexpr2
              | _ when env.is_in (to_expr env aexpr2_pos) ritv2 -> aexpr2
              | _ -> DelayedWrap (aexpr2_pos, op=O_plus, ritv2) in
            (BinExpr (Add, aexpr1, aexpr2), split_both m1 m2)
      in
      check_int_overflow env exp (reduce env aexpr, m, flow)

      (* 𝔼⟦ e * e' ⟧ and type(exp) = int *)
    | E_binop(O_mult, e, e') when exp |> etyp |> is_c_int_type ->
      let (aexpr1, m1, flow) = abstract env e flow in
      let (aexpr2, m2, flow) = abstract env e' flow in
      let (aexpr, m) =
        match (is_constant aexpr1, aexpr1, m1), (is_constant aexpr2, aexpr2, m2) with
        | (Some alpha1, _, (NoMod | Mod _)), (Some alpha2, _, (NoMod | Mod _)) ->
          (lf_const (Z.mul (apply_mod alpha1 m1) (apply_mod alpha2 m2)), NoMod)
        | (Some alpha, _, (NoMod | Mod _ as m_alpha)), (None, aexpr, Mod (l,u))
        | (None, aexpr, Mod (l,u)), (Some alpha, _, (NoMod | Mod _ as m_alpha)) ->
          let alpha' = apply_mod alpha m_alpha in
          if Z.lt Z.zero alpha' then
            (BinExpr (Mult, aexpr, lf_const alpha'), Mod (Z.mul l alpha', Z.mul u alpha'))
          else if Z.equal Z.zero alpha' then
            (lf_const Z.zero, NoMod)
          else
            (BinExpr (Mult, aexpr, lf_const alpha'), Mod (Z.succ (Z.mul u alpha'), Z.succ (Z.mul l alpha')))
        | _ ->
          try
            (BinExpr (Mult, rm_mod env (aexpr1, m1), rm_mod env (aexpr2, m2)), NoMod)
          with No_representation ->
            let rmin1, rmax1 = rangeof e.etyp flow in
            let ritv1 = IntItv.of_z rmin1 rmax1 in
            let aexpr1 = match m1 with
              | NoMod -> aexpr1
              | _ when env.is_in (to_expr env aexpr1) ritv1 -> aexpr1
              | _ -> DelayedWrap (aexpr1, true, ritv1) in
            let rmin2, rmax2 = rangeof e'.etyp flow in
            let ritv2 = IntItv.of_z rmin2 rmax2 in
            let aexpr2 = match m2 with
              | NoMod -> aexpr2
              | _ when env.is_in (to_expr env aexpr2) ritv2 -> aexpr2
              | _ -> DelayedWrap (aexpr2, true, ritv2) in
            (BinExpr (Mult, aexpr1, aexpr2), split_both m1 m2)
      in
      check_int_overflow env exp (reduce env aexpr, m, flow)

      (* 𝔼⟦ e / e' ⟧, type(exp) = int *)
    | E_binop(O_div, e, e') when exp |> etyp |> is_c_int_type ->
      let (aexpr1, m1, flow) = abstract env e flow in
      let (aexpr2, m2, flow) = abstract env e' flow in
      let nexp2 = (aexpr2, m2) |> rm_mod env |> to_expr env in
      let flow =
        let itv_denom = env.iota nexp2 in
        if IntItv.contains_zero itv_denom then
          raise No_representation (* first approximation *)
        else if exp.erange == env.range then
          flow (* if the expression is just a rewritten shift-right expression *)
        else
          safe_c_divide_by_zero_check exp.erange env.man flow
      in
      let (aexpr, m) =
        match is_constant aexpr2, m1, m2 with
        | Some alpha, Mod (l,u), (NoMod | Mod _) ->
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

    (* 𝔼⟦ (int)int ⟧ like in [Intraproc], this reduces the number of safe checks *)
    | E_c_cast(e, _) when is_c_type e.etyp && compare_typ exp.etyp e.etyp = 0 ->
      abstract env e flow

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
      | (Finite l, Finite u) when Z.equal l u ->
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
          let exp' = mk_binop ~etyp:exp.etyp e op' e' env.range in
          abstract env exp' flow
        else
          raise No_representation
      | _ -> raise No_representation
      end

    (* 𝔼⟦ e % e' ⟧, and type(exp) = int *)
    | E_binop(O_mod, e, e') when exp |> etyp |> is_c_int_type ->
      let (aexpr2, m2, flow) = abstract env e' flow in
      let aexpr2' = rm_mod env (aexpr2, m2) in
      (* when the modulo is *by a nonzero constant* and the numerator is either positive xor negative, transform the modulo *)
      begin match env.iota (to_expr env aexpr2') with
      | (Finite n, Finite n') when Z.equal n n' && not (Z.equal Z.zero n) ->
        let n = Z.abs n in
        let flow = safe_c_divide_by_zero_check exp.erange env.man flow in
        let (aexpr1, m1, flow) = abstract env e flow in
        let num_expr =
          match m1 with
          | Mod (l,u) -> wrap_expr (to_expr env aexpr1) (l, Z.pred u) env.range
          | NoMod | ToBeKSplit _ -> to_expr env aexpr1
        in
        let num_int = env.iota num_expr in
        let (aexpr, m) =
          if IntItv.is_positive num_int then
            (* if a >= 0, a % n = a mod [0,|n|[ *)
            apply_mod_aexpr env (aexpr1, m1) (Z.zero, n)
          else if IntItv.is_negative num_int then
            (* if a <= 0, a % n = a mod [-|n|+1,1[ *)
            apply_mod_aexpr env (aexpr1, m1) (Z.succ (Z.neg n), Z.one)
          else
            let aexpr = DelayedMod (aexpr1, true, n) in
            let m = match m1 with
              | NoMod -> ToBeKSplit n
              | Mod _ | ToBeKSplit _ -> ToBeKSplit (Z.gcd n (get_m_cardinal m1))
            in
            (aexpr, m)
        in
        check_int_overflow env exp (aexpr, m, flow)
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
        if IntItv.included int1 IntItv.zero_inf && IntItv.included int2 IntItv.zero_inf then
          reduce env (BinExpr (ConvexJoin, lf_const Z.zero, reduce env (BinExpr (op, e1, e2))))
        else if IntItv.included int1 IntItv.minf_zero && IntItv.included int2 IntItv.minf_zero then
          reduce env (BinExpr (ConvexJoin, lf_const Z.zero, reduce env (BinExpr (op, e1, e2))))
        else e
      | _ -> e
      end
    | BinExpr (Add, e1, e2) when e1 = opposite env e2 && is_deterministic e1 ->
      lf_const Z.zero
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
                  IntItv.included (env.iota (mk_binop ~etyp:T_int (mk_var x env.range) O_minus (mk_var a env.range) env.range)) IntItv.zero_inf &&
                  IntItv.included (env.iota (mk_binop ~etyp:T_int (mk_var b env.range) O_minus (mk_var x env.range) env.range)) IntItv.zero_inf &&
                  IntItv.included (env.iota (mk_binop ~etyp:T_int (mk_var b env.range) O_minus (mk_var a env.range) env.range)) one_inf ->
                let ez' = reduce env (BinExpr (Mult, lf_const (Z.divexact k1 k2), ez)) in

                (* if X<B and the result is named R, then |R| <= |ez'| - 1
                  (if ez'=0 it goes well because 0 is added afterward via the convex join) *)
                let ez' =
                  if IntItv.included (env.iota (mk_binop ~etyp:T_int (mk_var b env.range) O_minus (mk_var x env.range) env.range)) one_inf
                  then
                    let int = env.iota (to_expr env ez') in
                    if IntItv.included int IntItv.zero then
                      lf_const Z.zero
                    else if IntItv.included int IntItv.zero_inf then
                      reduce env (BinExpr (Add, lf_const Z.minus_one, ez'))
                    else if IntItv.included int IntItv.minf_zero then
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
            | (Finite mi, Finite ma) ->
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
                if IntItv.included int one_inf then
                  Some Z.one
                else if IntItv.included int minf_mone then
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
    | E_unop((O_plus | O_minus | O_bit_invert), e)
    | E_c_cast(e, _) ->
      is_supported_expr e
    | E_binop((O_plus | O_minus | O_mult | O_div | O_convex_join | O_bit_lshift | O_bit_rshift | O_mod), e, e') ->
      is_supported_expr e && is_supported_expr e'
    | _ -> false

  (** Translate comparison of integer expressions into comparison to [0],
      otherwise call [abstract] and try to remove the modular ring in which the abstract expression is evaluated.
      If this fails, [No_representation] is raised.  *)
  let abstract_with_comparisons env exp flow =
    match ekind exp with
      (* 𝔼⟦ e ⋄ e' ⟧, ⋄ ∈ {<, <=, >, >=, ==, !=} *)
      | E_binop((O_gt | O_ge | O_lt | O_le | O_eq | O_ne) as comp_op, e, e') when is_supported_expr e && is_supported_expr e' ->
        let (aexpr1, m1, flow) = abstract env e flow in
        let (aexpr2, m2, flow) = abstract env e' flow in
        let zero = mk_int 0 ~typ:T_int env.range in
        begin match m1, m2 with
        | Mod (l1,u1), Mod (l2,u2) when
            Z.equal l1 l2 &&
            Z.equal u1 u2 &&
            is_constant (reduce env (BinExpr (Add, aexpr1, opposite env aexpr2))) = Some Z.zero ->
          (mk_binop ~etyp:T_int zero comp_op zero env.range, flow) (* rule PlusEqZero *)
        | _ ->
          let aexpr = reduce env (BinExpr (Add, rm_mod env (aexpr1, m1), opposite env (rm_mod env (aexpr2, m2)))) in
          (mk_binop ~etyp:T_int (to_expr env aexpr) comp_op zero env.range, flow)
        end
      | _ when is_supported_expr exp ->
        let (aexpr,m,flow') = abstract env exp flow in
        begin try
          let e' = to_expr env (rm_mod env (aexpr, m)) in
          (e', flow')
        with No_representation ->
          let () = debug "Could have rewritten expression %a into %a%a." pp_expr exp pp_expr (to_expr env aexpr) pp_mod m in
          raise No_representation
        end
      | _ ->
        raise No_representation


  (** {2 Transfer functions} *)
  (** ====================== *)

  let eval exp (man: ('a,'b) man) flow =
    (* abort trying to rewrite expressions if the abstract state is bottom *)
    if man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then None else
    
    match exp.etyp with
    | T_c_integer _ ->

      (* rewriting environment *)
      let iota (e: expr): IntItv.t =
        try
          begin match ask_and_reduce man.ask (Universal.Numeric.Common.mk_int_interval_query ~fast:false e) flow with
          | BOT -> raise No_representation
          | Nb int -> int
          end
        with Not_found ->
          let () = debug "Couldn't query interval of %a, the backtrace is:" pp_expr e in
          let () = if Debug.can_print debug_channel then Printexc.print_backtrace stdout in
          raise No_representation
      in
      let is_var_constant (v: var): Z.t option =
        let e = mk_var v dummy_range in
        match ask_and_reduce man.ask (Universal.Numeric.Common.mk_int_interval_query ~fast:true e) flow with
        | BOT -> raise No_representation
        | Nb (Finite l, Finite u) when Z.equal l u -> Some l
        | exception Not_found
        | _ -> None
      in
      let is_in (e: expr) (target: IntItv.t): bool =
        try
          begin match ask_and_reduce man.ask (Universal.Numeric.Common.mk_int_interval_query ~fast:true e) flow with
          | BOT -> raise No_representation
          | Nb int_no_rel when IntItv.included int_no_rel target -> true
          | _ ->
            match ask_and_reduce man.ask (Universal.Numeric.Common.mk_int_interval_query ~fast:false e) flow with
            | BOT -> raise No_representation
            | Nb int -> IntItv.included int target
          end
        with Not_found ->
          let () = debug "Couldn't query interval of %a, the backtrace is:" pp_expr e in
          let () = if Debug.can_print debug_channel then Printexc.print_backtrace stdout in
          raise No_representation
      in
      let env: ('a,'b) env = {
          iota;
          is_var_constant;
          is_in;
          man;
          range = mk_tagged_range (String_tag "c.symbolic_rewriting") exp.erange;
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

  let init _ _ flow =
    debug "Enable the \"-enforce-sign-constraints\" option of the relational domain";
    Relational.Domain.opt_enforce_sign_constraints := true;
    flow

  let ask _ _ _ = None
  let exec _ _ _ = None
  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
