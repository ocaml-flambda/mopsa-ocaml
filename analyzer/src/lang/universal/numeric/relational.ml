(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Relational numeric abstract domain, based on APRON. *)

open Framework.Essentials
open Framework.Domains.Leaf
open Ast


(****************************************************************************)
(**                      {2 Command line options}                           *)
(****************************************************************************)

let opt_float_rounding = ref Apron.Texpr1.Near

let () =
  register_option (
    "-float-rounding-mode",
    Arg.String (function
        | "near" -> opt_float_rounding := Apron.Texpr1.Near
        | "zero" -> opt_float_rounding := Apron.Texpr1.Zero
        | "up"   -> opt_float_rounding := Apron.Texpr1.Up
        | "down" -> opt_float_rounding := Apron.Texpr1.Down
        | "rnd"  -> opt_float_rounding := Apron.Texpr1.Rnd
        | x -> Debug.fail "Unknown rounding mode %s" x
      ),
    "selects the rounding mode of floating-point computations. Possible values: near, zero, up, down, and rnd (default: near)."
  )


(****************************************************************************)
(**                         {2 Abstract domain}                             *)
(****************************************************************************)

(** Module to encapsulate a manager of Apron and the type of its abstraction *)
module type APRONMANAGER =
sig
  type t
  val man : t Apron.Manager.t
  val name : string
end

module Make(ApronManager : APRONMANAGER) =
struct

  type t = ApronManager.t Apron.Abstract1.t

  type _ domain += D_universal_relational : t domain

  let id = D_universal_relational
  let name = "universal.numeric.relational." ^ ApronManager.name

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_universal_relational -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** {2 Environment utility functions} *)
  (** ********************************* *)

  let empty_env = Apron.Environment.make [||] [||]

  let unify abs1 abs2 =
    let env1 = Apron.Abstract1.env abs1 and env2 = Apron.Abstract1.env abs2 in
    let env = Apron.Environment.lce env1 env2 in
    (Apron.Abstract1.change_environment ApronManager.man abs1 env false),
    (Apron.Abstract1.change_environment ApronManager.man abs2 env false)

  let var_to_apron v =
    (match v.vtyp with
    | T_int -> Format.fprintf Format.str_formatter "%s#%d" v.vname v.vuid;
    | T_float -> Format.fprintf Format.str_formatter "%s@%d" v.vname v.vuid;
    | _ -> assert false);
    let name = Format.flush_str_formatter () in
    Apron.Var.of_string name

  let apron_to_var v =
    let v = Apron.Var.to_string v in
    if Str.string_match (Str.regexp "\\([^#]+\\)#\\([0-9]+\\)") v 0 then
      let vname = Str.matched_group 1 v in
      let vuid = Str.matched_group 2 v |> int_of_string in
      {vname; vuid; vtyp = T_int}
    else
    if Str.string_match (Str.regexp "\\([^@]+\\)@\\([0-9]+\\)") v 0 then
      let vname = Str.matched_group 1 v in
      let vuid = Str.matched_group 2 v |> int_of_string in
      {vname; vuid; vtyp = T_float}
    else raise (Invalid_argument v)

  let is_env_var v abs =
    let env = Apron.Abstract1.env abs in
    Apron.Environment.mem_var env (var_to_apron v)

  let add_missing_vars  abs lv =
    let env = Apron.Abstract1.env abs in
    let lv = List.sort_uniq compare lv in
    let lv = List.filter (fun v -> not (Apron.Environment.mem_var env (var_to_apron v))) lv in
    let env' = Apron.Environment.add env
        (
          Array.of_list @@
          List.map var_to_apron @@
          List.filter (function {vtyp = T_int} -> true | _ -> false) lv
        )
        (
          Array.of_list @@
          List.map var_to_apron @@
          List.filter (function {vtyp = T_float} -> true | _ -> false) lv
        )
    in
    Apron.Abstract1.change_environment ApronManager.man abs env' false


  (** {2 Lattice operators} *)
  (** ********************* *)

  let top = Apron.Abstract1.top ApronManager.man empty_env

  let bottom = Apron.Abstract1.bottom ApronManager.man empty_env

  let is_bottom abs =
    Apron.Abstract1.is_bottom ApronManager.man abs

  let subset abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.is_leq ApronManager.man abs1' abs2'

  let join annot abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.join ApronManager.man abs1' abs2'

  let meet annot abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.meet ApronManager.man abs1' abs2'

  let widen annot abs1 abs2 =
    let abs1', abs2' = unify abs1 abs2 in
    Apron.Abstract1.widening ApronManager.man abs1' abs2'

  let print fmt abs =
    Format.fprintf fmt "%s:@\n  @[%a@]@\n"
      ApronManager.name
      Apron.Abstract1.print abs


  (** {2 Transformers to Apron syntax} *)
  (** ******************************** *)

  exception UnsupportedExpression
  
  let rec binop_to_apron = function
    | O_plus  -> Apron.Texpr1.Add
    | O_minus  -> Apron.Texpr1.Sub
    | O_mult  -> Apron.Texpr1.Mul
    | O_div  -> Apron.Texpr1.Div
    | O_mod  -> Apron.Texpr1.Mod
    | _ -> raise UnsupportedExpression

  and exp_to_apron exp =
    match ekind exp with
    | E_constant(C_int_interval (a,b)) ->
       Apron.Texpr1.Cst(
         Apron.Coeff.i_of_scalar
           (Apron.Scalar.of_float @@ Z.to_float a)
           (Apron.Scalar.of_float @@ Z.to_float b)
       )

    | E_constant(C_float_interval (a,b)) ->
      Apron.Texpr1.Cst(
        Apron.Coeff.i_of_scalar
          (Apron.Scalar.of_float a)
          (Apron.Scalar.of_float b)
      )

    | E_constant(C_int n) -> Apron.Texpr1.Cst(Apron.Coeff.Scalar(Apron.Scalar.of_float @@ Z.to_float n))

    | E_constant(C_float f) -> Apron.Texpr1.Cst(Apron.Coeff.Scalar(Apron.Scalar.of_float f))

    | E_var (x) -> Apron.Texpr1.Var(var_to_apron x)

    | E_binop(binop, e1, e2) ->
       let binop' = binop_to_apron binop in
       let e1' = exp_to_apron e1 and e2' = exp_to_apron e2 in
       let typ' = typ_to_apron exp.etyp in
       Apron.Texpr1.Binop(binop', e1', e2', typ', !opt_float_rounding)

    | E_unop(O_minus , e) ->
      let e' = exp_to_apron e in
      let typ' = typ_to_apron e.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Neg, e', typ', !opt_float_rounding)

    | E_unop(O_sqrt, e) ->
      let e' = exp_to_apron e in
      let typ' = typ_to_apron T_float in
      Apron.Texpr1.Unop(Apron.Texpr1.Sqrt, e', typ', !opt_float_rounding)

    | E_unop(O_wrap(g, d), e) ->
      let r = erange e in
      mk_binop (mk_z g r) O_plus (mk_binop
                                    (mk_binop e O_minus (mk_z g r) r)
                                    O_mod
                                    (mk_z (Z.(d-g+one)) r)
                                    r
                                 ) r
      |> exp_to_apron

    | _ ->
      warn "[exp_to_apron] : failed to transform %a of type %a" pp_expr exp pp_typ (etyp exp);
      raise UnsupportedExpression

  and typ_to_apron = function
    | T_int -> Apron.Texpr1.Int
    | T_float -> Apron.Texpr1.Real
    | _ -> assert false

  and bexp_to_apron exp =
    match ekind exp with
    | E_constant(C_int n) when Z.to_int n = 0 -> Dnf.mk_false

    | E_constant(C_int _) -> Dnf.mk_true

    | E_binop(O_gt, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.SUP, e0', e0.etyp, e1', e1.etyp)

    | E_binop(O_ge, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.SUPEQ, e0', e0.etyp, e1', e1.etyp)

    | E_binop(O_lt, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.SUP, e1', e1.etyp, e0', e0.etyp)

    | E_binop(O_le, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.SUPEQ, e1', e1.etyp, e0', e0.etyp)

    | E_binop(O_eq, e0 , e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.singleton (Apron.Tcons1.EQ, e0', e0.etyp, e1', e1.etyp)

    | E_binop(O_ne, e0, e1) ->
       let e0' = exp_to_apron e0 and e1' = exp_to_apron e1 in
       Dnf.mk_or
         (Dnf.singleton (Apron.Tcons1.SUP, e0', e0.etyp, e1', e1.etyp))
         (Dnf.singleton (Apron.Tcons1.SUP, e1', e1.etyp, e0', e0.etyp))

    | E_binop(O_log_or, e1, e2) ->
      Dnf.mk_or (bexp_to_apron e1) (bexp_to_apron e2)

    | E_binop(O_log_and,e1, e2) ->
      Dnf.mk_and (bexp_to_apron e1) (bexp_to_apron e2)

    | E_unop(O_log_not, exp') ->
      bexp_to_apron exp' |>
      Dnf.mk_neg (fun (op, e1, t1, e2, t2) ->
          match op with
          | Apron.Tcons1.EQ ->
            Dnf.mk_or
              (Dnf.singleton (Apron.Tcons1.SUP, e1, t1, e2, t2))
              (Dnf.singleton (Apron.Tcons1.SUP, e2, t2, e1, t1))
          | Apron.Tcons1.SUP ->
            Dnf.singleton (Apron.Tcons1.SUPEQ, e2, t2, e1, t1)
          | Apron.Tcons1.SUPEQ ->
            Dnf.singleton (Apron.Tcons1.SUP, e2, t2, e1, t1)
          | _ -> assert false
        )

    | _ ->
       let e0' = exp_to_apron exp in
       let e1' = Apron.Texpr1.Cst(Apron.Coeff.s_of_int 0) in
       Dnf.mk_or
         (Dnf.singleton (Apron.Tcons1.SUP, e0', exp.etyp, e1', T_int))
         (Dnf.singleton (Apron.Tcons1.SUP, e1', T_int, e0', exp.etyp))

  and tcons_array_of_tcons_list env l =
    let n = List.length l in
    let cond_array = Apron.Tcons1.array_make env n in
    let () = List.iteri (fun i c ->
      Apron.Tcons1.array_set cond_array i c;
    ) l in
    cond_array
  

  (** {2 Transfer functions} *)
  (** ********************** *)

  let zone = Zone.Z_universal_num
  
  let init prog = top

  let rec exec stmt a =
    match skind stmt with
    | S_remove_var var ->
      let env = Apron.Abstract1.env a in
      let vars =
        List.filter (fun v -> is_env_var v a) [var] |>
        List.map var_to_apron
      in
      let env = Apron.Environment.remove env (Array.of_list vars) in
      Apron.Abstract1.change_environment ApronManager.man a env true |>
      return

    | S_rename_var( v, v') ->
        Apron.Abstract1.rename_array ApronManager.man a
          [| var_to_apron v  |]
          [| var_to_apron v' |] |>
        return

    | S_project_vars vars ->
      let env = Apron.Abstract1.env a in
      let vars = List.map var_to_apron vars in
      let old_vars1, old_vars2 = Apron.Environment.vars env in
      let old_vars = Array.to_list old_vars1 @ Array.to_list old_vars2 in
      let to_remove = List.filter (fun v -> not (List.mem v vars)) old_vars in
      let new_env = Apron.Environment.remove env (Array.of_list to_remove) in
      Apron.Abstract1.change_environment ApronManager.man a new_env true |>
      return

    | S_assign({ekind = E_var v}, e, STRONG) ->
      let a = add_missing_vars a (v :: (Framework.Visitor.expr_vars e)) in
      begin try
          let aenv = Apron.Abstract1.env a in
          let texp = Apron.Texpr1.of_expr aenv (exp_to_apron e) in
          Apron.Abstract1.assign_texpr ApronManager.man a (var_to_apron v) texp None |>
          return
        with UnsupportedExpression ->
          exec {stmt with skind = S_remove_var v} a
      end

    | S_assign({ekind = E_var v}, e, WEAK) ->
      panic "relational: weak updates not yet supported"

    | S_assign(({ekind = E_var x}), e, EXPAND) ->
      panic "relational: expand not yet supported"

    | S_assume(e) -> begin
        let a = add_missing_vars  a (Framework.Visitor.expr_vars e) in
        let env = Apron.Abstract1.env a in

        let join_list l = List.fold_left (Apron.Abstract1.join ApronManager.man) (Apron.Abstract1.bottom ApronManager.man env) l in
        let meet_list l = tcons_array_of_tcons_list env l |>
                        Apron.Abstract1.meet_tcons_array ApronManager.man a
        in

        try
          bexp_to_apron e |>
          Dnf.apply
            (fun (op,e1,typ1,e2,typ2) ->
                let typ =
                  match typ1, typ2 with
                  | T_int, T_int -> Apron.Texpr1.Int
                  | T_float, T_int
                  | T_int, T_float
                  | T_float, T_float -> Apron.Texpr1.Real
                  | _ -> fail "Unsupported case (%a, %a) in stmt @[%a@]" pp_typ typ1 pp_typ typ2 pp_stmt stmt
                in
                let diff = Apron.Texpr1.Binop(Apron.Texpr1.Sub, e1, e2, typ, !opt_float_rounding) in
                let diff_texpr = Apron.Texpr1.of_expr env diff in
                Apron.Tcons1.make diff_texpr op
            )
            meet_list join_list |>
          return
        with UnsupportedExpression ->
          return a
      end

    | _ -> return top

  and ask query a = None


  let z_of_z2 z z' round =
    let open Z in
    let d, r = div_rem z z' in
    if equal r zero then
      d
    else
      begin
        if round then
          d + one
        else
          d
      end

  let z_of_mpzf mp =
    Z.of_string (Mpzf.to_string mp)

  let z_of_mpqf mp round =
    let open Mpqf in
    let l, r = to_mpzf2 mp in
    let lz, rz = z_of_mpzf l, z_of_mpzf r in
    z_of_z2 lz rz round

  let z_of_apron_scalar a r =
    let open Apron.Scalar in
    match a, r with
    | Float f, true  -> Z.of_float (ceil f)
    | Float f, false -> Z.of_float (floor f)
    | Mpqf q, _ ->  z_of_mpqf q r
    | Mpfrf mpf, _ -> z_of_mpqf (Mpfr.to_mpq mpf) r
  
  let var_relations v a =
    (* Get the linear constraints *)
    let lincons_list =
      let earray = Apron.Abstract1.to_lincons_array ApronManager.man a in
      let rec iter i = if i == Apron.Lincons1.array_length earray then [] else (Apron.Lincons1.array_get earray i) :: (iter (i + 1)) in
      iter 0
    in

    List.fold_left (fun acc lincons ->
        let t_involved = ref false in
        Apron.Lincons1.iter (fun c v' -> 
            t_involved := !t_involved || ((compare_var v (apron_to_var v') = 0) && not (Apron.Coeff.is_zero c))
          ) lincons;
        (* If lincons is involved in the constraint, we keep all other variables with non null coefficients *)
        if !t_involved then
          let vars = ref [] in
          Apron.Lincons1.iter (fun c v' ->
              let v' = apron_to_var v' in
              if compare_var v v' <> 0 && not (Apron.Coeff.is_zero c) then
                vars := v' :: !vars
            ) lincons;
          !vars @ acc
        else
          acc
      ) [] lincons_list |>
    List.sort_uniq compare_var


  let interval (v:var) (a:t) : (Values.Intervals.Value.t) =
    let itv = Apron.Abstract1.bound_variable ApronManager.man a (var_to_apron v) in
    if Apron.Interval.is_bottom itv then
      Values.Intervals.Value.bottom
    else
      let mi = itv.Apron.Interval.inf in
      let ma = itv.Apron.Interval.sup in
      let to_b m r =
        let x = Apron.Scalar.is_infty m in
        if x = 0 then Values.Intervals.Value.I.B.Finite (z_of_apron_scalar m r)
        else if x > 0 then Values.Intervals.Value.I.B.PINF
        else Values.Intervals.Value.I.B.MINF
      in
      Bot.Nb (to_b mi false, to_b ma true)



end


module Oct = Make(struct type t = Oct.t let name = "octagon" let man = Oct.manager_alloc () end)
module Poly = Make(struct type t = Polka.strict Polka.t let name = "polyhedra" let man = Polka.manager_alloc_strict () end)

let () =
  register_domain (module Oct);
  register_domain (module Poly);
