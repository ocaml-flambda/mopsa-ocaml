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


(** ToolBox module for Apron interfacing *)

open Mopsa
open Rounding
open Ast
open Apron_manager



module ApronTransformer(ApronManager : APRONMANAGER) =
struct

  let debug fmt = Debug.debug ~channel:"universal.numeric.relational.apron_transformer" fmt

  let var_to_apron (v:var) =
    let name = uniq_vname v in
    Apron.Var.of_string name

  (** FIXME: this is ugly! we need a flow-insensitive context to maintain a
      mapping mopsa var <-> apron var *)
  let apron_to_var v =
    let v = Apron.Var.to_string v in
    if Str.string_match (Str.regexp "\\([^:]+\\):\\([0-9]+\\)") v 0 then
      let orig = Str.matched_group 1 v in
      let vuid = Str.matched_group 2 v |> int_of_string in
      let uniq = orig ^ ":" ^ (string_of_int vuid) in
      mkv orig uniq vuid T_int
    else
    if Str.string_match (Str.regexp "\\([^@]+\\)@\\([0-9]+\\)") v 0 then
      let orig = Str.matched_group 1 v in
      let vuid = Str.matched_group 2 v |> int_of_string in
      let uniq = orig ^ ":" ^ (string_of_int vuid) in
      (* TODO: replace F_REAL with real type *)
      mkv orig uniq vuid (T_float F_REAL)
    else raise (Invalid_argument v)


  let get_interval (v:var) (a: ApronManager.t Apron.Abstract1.t) =
    Apron.Abstract1.bound_variable ApronManager.man a (var_to_apron v) |>
    Values.Intervals.Integer.Value.of_apron

  let is_numerical_var (v: var): bool =
    match vtyp v with
    | T_bool | T_int | T_float _ -> true
    | _ -> false
  let empty_env = Apron.Environment.make [| |] [| |]

  let print_env = Apron.Environment.print
      ~first:("[")
      ~sep:(",")
      ~last:("]")

  let filter_env int_filter real_filter env =
    let int_var, real_var = Apron.Environment.vars env in
    let list_int_var_to_keep =
      Array.fold_left (fun list_int_var_to_keep int_var ->
          if int_filter int_var then
            int_var :: list_int_var_to_keep
          else list_int_var_to_keep
        ) [] int_var
    in
    let list_real_var_to_keep =
      Array.fold_left (fun list_real_var_to_keep real_var ->
          if real_filter real_var then
            real_var :: list_real_var_to_keep
          else list_real_var_to_keep
        ) [] real_var
    in
    let array_int_res = Array.of_list list_int_var_to_keep in
    let array_real_res = Array.of_list list_real_var_to_keep in
    Apron.Environment.make array_int_res array_real_res

  let fold_env f env acc =
    let int_var, real_var = Apron.Environment.vars env in
    let acc' = Array.fold_left (fun acc x -> f x acc) acc int_var in
    Array.fold_left (fun acc x -> f x acc) acc' real_var

  let exists_env f env =
    let exception Exists in
    try
      let () = fold_env (fun e () -> if f e then raise Exists) env () in
      false
    with
    | Exists -> true

  let gce a b =
    filter_env
      (fun int_var -> Apron.Environment.mem_var b int_var)
      (fun int_var -> Apron.Environment.mem_var b int_var)
      a

  let diff a b =
    filter_env
      (fun int_var -> not (Apron.Environment.mem_var b int_var))
      (fun int_var -> not (Apron.Environment.mem_var b int_var))
      a

  let to_constraints a =
    let earray = Apron.Abstract1.to_lincons_array ApronManager.man a in
    let rec iter i =
      if i = Apron.Lincons1.array_length earray then []
      else
        let l = ref [] in
        let cons = Apron.Lincons1.array_get earray i in
        Apron.Lincons1.iter (fun c v ->
            l := (c, apron_to_var v) :: !l
          ) cons
        ;
        (!l, Apron.Lincons1.get_cst cons, Apron.Lincons1.get_typ cons) :: (iter (i + 1))
    in
    iter 0


  (** Restrict linear constraints involving variable [v] *)
  let constraints_of_var v constraints =
    List.filter (fun (cons,_,_) ->
        List.exists (fun (c, v') ->
            (compare_var v  v' = 0)
            && not (Apron.Coeff.is_zero c)
          ) cons
      ) constraints

  (** Get the list of variables with which [v] has numeric relations *)
  let related_vars v a =
    debug "computing related vars of %a in %a" pp_var v Apron.Abstract1.print a;
    to_constraints a |>
    constraints_of_var v |>
    List.fold_left (fun acc (cons,_,_) ->
        List.fold_left (fun acc (c, v') ->
            if compare_var v v' <> 0 && not (Apron.Coeff.is_zero c) then
              let () = debug "adding %a" pp_var v' in
              v' :: acc
            else
              acc
          ) acc cons
      ) []

  (** Get the list of constant variables *)
  let constant_vars a =
    debug "computing constant vars in %a" Apron.Abstract1.print a;
    let rec iter candidate l =
      match l with
      | [] -> candidate
      | (c, v) :: tl ->
        if Apron.Coeff.is_zero c
        then iter candidate tl
        else
          match candidate with
          | Some vv -> None
          | None -> iter (Some v) tl
    in
    to_constraints a |>
    List.fold_left (fun acc (cons,c,t) ->
        match t with
        | Apron.Lincons1.EQ ->
          begin
            match iter None cons with
            | None -> acc
            | Some v ->
              debug "%a is constant" pp_var v;
              v :: acc
          end
        | _ -> acc
      ) []



  exception UnsupportedExpression

  let rec binop_to_apron = function
    | O_plus  -> Apron.Texpr1.Add
    | O_minus -> Apron.Texpr1.Sub
    | O_mult  -> Apron.Texpr1.Mul
    | O_div   -> Apron.Texpr1.Div
    | O_mod   -> Apron.Texpr1.Mod
    | _ -> raise UnsupportedExpression

  and strongify_rhs exp abs l =
    match ekind exp with
    | E_constant(C_int_interval (a,b)) ->
      Apron.Texpr1.Cst(
        Apron.Coeff.i_of_scalar
          (Apron.Scalar.of_float @@ Z.to_float a)
          (Apron.Scalar.of_float @@ Z.to_float b)
      ), abs, l

    | E_constant(C_float_interval (a,b)) ->
      Apron.Texpr1.Cst(
        Apron.Coeff.i_of_scalar
          (Apron.Scalar.of_float a)
          (Apron.Scalar.of_float b)
      ), abs, l

    | E_constant(C_int n) ->
      Apron.Texpr1.Cst(Apron.Coeff.Scalar(Apron.Scalar.of_float @@ Z.to_float n)),
      abs, l

    | E_constant(C_float f) -> Apron.Texpr1.Cst(Apron.Coeff.Scalar(Apron.Scalar.of_float f)), abs, l

    | E_var (x, STRONG) ->
      Apron.Texpr1.Var(var_to_apron x), abs, l

    | E_var (x, WEAK) ->
      let x' = mktmp ~typ:exp.etyp () in
      let x_apr = var_to_apron x in
      let x_apr' = var_to_apron x' in
      let abs = Apron.Abstract1.expand ApronManager.man abs x_apr [| x_apr' |] in
      (Apron.Texpr1.Var x_apr, abs, x_apr' :: l)

    | E_binop(binop, e1, e2) ->
      let binop' = binop_to_apron binop in
      let e1', abs, l = strongify_rhs e1 abs l in
      let e2', abs, l = strongify_rhs e2 abs l in
      let typ' = typ_to_apron exp.etyp in
      Apron.Texpr1.Binop(binop', e1', e2', typ', !opt_float_rounding), abs, l

    | E_unop (O_plus, e) ->
      strongify_rhs e abs l

    | E_unop(O_cast, e) ->
      let e', abs, l = strongify_rhs e abs l in
      let typ' = typ_to_apron e.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Cast, e', typ', !opt_float_rounding), abs, l

    | E_unop(O_minus, e) ->
      let e', abs, l = strongify_rhs e abs l in
      let typ' = typ_to_apron e.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Neg, e', typ', !opt_float_rounding), abs, l

    | E_unop(O_sqrt, e) ->
      let e', abs, l = strongify_rhs e abs l in
      let typ' = typ_to_apron exp.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Sqrt, e', typ', !opt_float_rounding), abs, l

    | E_unop(O_wrap(g, d), e) ->
      let r = erange e in
      mk_binop ~etyp:T_int
        (mk_z g r) O_plus (mk_binop ~etyp:T_int
                             (mk_binop e O_minus (mk_z g r) r ~etyp:T_int)
                             O_mod
                             (mk_z (Z.(d-g+one)) r)
                             r
                          ) r
      |> fun x -> strongify_rhs x abs l

    | _ ->
      Exceptions.warn "[strongify rhs] : failed to transform %a of type %a" pp_expr exp pp_typ (etyp exp);
      raise UnsupportedExpression

  and is_env_var v abs =
    let env = Apron.Abstract1.env abs in
    Apron.Environment.mem_var env (var_to_apron v)

  and is_env_var_apron v abs =
    let env = Apron.Abstract1.env abs in
    Apron.Environment.mem_var env v

  and remove_tmp tmpl abs =
    let env = Apron.Abstract1.env abs in
    let vars =
      List.filter (fun v -> is_env_var_apron v abs) tmpl
    in
    let env = Apron.Environment.remove env (Array.of_list vars) in
    Apron.Abstract1.change_environment ApronManager.man abs env true

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

    | E_var (v, _) ->
      Apron.Texpr1.Var(var_to_apron v)

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
      let typ' = typ_to_apron exp.etyp in
      Apron.Texpr1.Unop(Apron.Texpr1.Sqrt, e', typ', !opt_float_rounding)

    | E_unop(O_wrap(g, d), e) ->
      let r = erange e in
      mk_binop ~etyp:T_int
        (mk_z g r)
        O_plus
        (mk_binop ~etyp:T_int
           (mk_binop e O_minus (mk_z g r) r ~etyp:T_int)
           O_mod
           (mk_z (Z.(d-g+one)) r)
           r
        ) r
      |> exp_to_apron

    | _ ->
      Exceptions.warn "[exp_to_apron] : failed to transform %a of type %a" pp_expr exp pp_typ (etyp exp);
      raise UnsupportedExpression

  and typ_to_apron = function
    | T_bool -> Apron.Texpr1.Int
    | T_int -> Apron.Texpr1.Int
    | T_float F_SINGLE -> Apron.Texpr1.Single
    | T_float F_DOUBLE -> Apron.Texpr1.Double
    | T_float F_LONG_DOUBLE -> Apron.Texpr1.Extended
    | T_float F_REAL -> Apron.Texpr1.Real
    | t -> panic ~loc:__LOC__ "typ_to_apron: unsupported type %a" pp_typ t

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

  let get_interval_expr (e:expr) (a: ApronManager.t Apron.Abstract1.t) =
    Apron.Abstract1.bound_texpr ApronManager.man a
      (exp_to_apron e |> Apron.Texpr1.of_expr (Apron.Abstract1.env a)) |>
      Values.Intervals.Integer.Value.of_apron

end
